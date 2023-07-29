library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(purrr)
library(modelr)
source("exec/util.R")

setwd("/Users/noashapiro-tamir/Documents/dev/DecompProject")

#Importing data
df <- read.csv("data/decomp_data.csv")
sitedf <- read.csv("data/site_contents.csv")[,c(1,4)]
initials <- read.csv("data/initials.csv")
df <- merge(df, sitedf, by = "tag")
df$SLC <- interaction(as.factor(df$site), as.factor(df$class))
df$PercMassRemaining <- 1 - (df$init_total_mass - df$post_total_mass)/df$init_mass_litter
df$post_litter_mass = df$init_mass_litter - df$mass_loss
df <- bind_rows(df, initials)
df[117, 3] = "Phragmites"
#Changing the days_to_collection values to reflect accurate collection dates
df <- df %>%
  mutate(days_to_collection = if_else(days_to_collection == 28, 22, days_to_collection))%>%
  mutate(days_to_collection = if_else(days_to_collection == 84, 76, days_to_collection))%>%
  mutate(days_to_collection = if_else(days_to_collection == 182, 163, days_to_collection))%>%
  mutate(days_to_collection = if_else(days_to_collection == 365, 345, days_to_collection))
         
#visualizing the weird bump at the third collection date
ggplot(df, aes(x = days_to_collection, y = PercMassRemaining)) + geom_point()
boxplot(df$PercMassRemaining ~ df$days_to_collection, outlier_tagging = TRUE)

find_outliers_df <- df%>%
  filter(SLC == "Mo3.Morella")

# direct estimate of alpha for each measurement point
# this function lives in "exec/util.R" and is read in via the source() call above
df$alpha_est <- get_alpha(df$PercMassRemaining, df$days_to_collection)

# plotting alpha estimates.  Notice that they get more constrained over time. 
# in a perfect dataset with no noise, you'd expect that each point within a group/treatment/etc
    # produces the exact same alpha -- it should be a horizontal line over time. 
ggplot(data = df) + 
  aes(x = days_to_collection, y = alpha_est) +
  geom_point() + 
  facet_wrap(vars(treatment))

# here's a NLS example without self-starting.
cool_model <- nls(
  post_litter_mass ~ init_mass_litter * exp(-a * days_to_collection), 
  data = df %>% filter(treatment == "Morella"),
  start = list(a = 0.002)
)
cool_model

# here's a sanity check for the estimate of alpha that it spits out
# you'll need to do the actual stats on it but here's a gander
# get_mass_pct_remaining_at_t is also a function from "exec/util.R"
t_to_eyeball_fit <- 1:365
perc_mass_to_eyeball_fit <- get_mass_pct_remaining_at_t(
  a = coef(cool_model)[['a']], 
  t = t_to_eyeball_fit
)
plot(
  x = df[which(df$treatment == "Morella"),"days_to_collection"], 
  y = df[which(df$treatment == "Morella"),"PercMassRemaining"]
)
lines(
  x = t_to_eyeball_fit,
  y = perc_mass_to_eyeball_fit,
  t = "p",
  col = "blue"
)

#Noa makes the same model but with percents. Has a lower residual sum of squares so kept it. Want to double check the 
noa_model <- nls(
  PercMassRemaining ~ 1* exp(-a * days_to_collection),
  data = df %>% filter(treatment == "Morella"),
  start = list(a = 0.0008)
)

noa_model


#Testing Noa model - trying to do here what Riley did with Cool model. Not sure if it actually shows us new info or not.
t_to_eyeball_fit <- 1:365
perc_mass_to_eyeball_fit_noa <- get_mass_pct_remaining_at_t(
  a = coef(noa_model)[['a']], 
  t = t_to_eyeball_fit
)
plot(
  x = df[which(df$treatment == "Morella"),"days_to_collection"], 
  y = df[which(df$treatment == "Morella"),"PercMassRemaining"]
)
lines(
  x = t_to_eyeball_fit,
  y = perc_mass_to_eyeball_fit_noa,
  t = "p",
  col = "blue"
)

#trying to do self-starting model... IT WORKS NOW!
fit_morella <- nls(PercMassRemaining ~ SSasymp(days_to_collection, yf, y0, log_alpha), data = df %>% filter(treatment == "Morella"))

fit_morella

#Plotting decay curve of non-iterated self-starting model
    #this first one doesn't work yet
grid_morella <- data.frame(x = seq(0, 345, length = 346))
grid_morella %>% add_predictions(fit_morella)

  plot(df$PercMassRemaining ~ df$days_to_collection)
curve(predict(fit_morella, grid_morella))

  #this one should work
df%>% filter(treatment == "Morella") %>% 
  add_predictions(fit_morella) %>%
  ggplot(aes(x = days_to_collection, y = PercMassRemaining, group = treatment)) + 
  geom_point() + 
  geom_line(aes(x=days_to_collection, y=pred)) + 
  ggtitle("Decay Curve: Morella Treatment")

#Iterating noa_model for all treatments - BAD ALPHAS
treatment_alphas <- df %>% 
  nest(-treatment) %>%
  mutate(
    fit = map(data, ~nls(PercMassRemaining ~ 1* exp(-a * days_to_collection), data = ., start = list(a = 0.1))),
    tidied = map(fit, tidy),
    Augmented = map(fit, augment),)

treatment_alphas_tbl <- treatment_alphas %>% 
  unnest(tidied) %>% 
  select(treatment, term, estimate) %>% 
  spread(term, estimate)

treatment_alphas_tbl


ggplot(treatment_alphas_ss_tbl, aes(x = treatment, y = log_alpha)) + geom_bar(stat = "identity", width = 0.5) + 
  geom_errorbar(aes(ymin = log_alpha-std.error, ymax = log_alpha+std.error), width = 0.2)


#Iterating noa_model for all Site/Treatment combos - BAD ALPHAS
SLC_alphas <- df %>% 
  nest(-SLC) %>%
  mutate(
    fit = map(data, ~nls(PercMassRemaining ~ 1* exp(-a * days_to_collection), data = ., start = list(a = 0.01))),
    tidied = map(fit, tidy),
    Augmented = map(fit, augment),)

SLC_alphas_tbl <- 
  SLC_alphas %>% 
  unnest(tidied) %>% 
  select(SLC, term, estimate) %>% 
  spread(term, estimate)

SLC_alphas_tbl

#trying to get the self-starter to iterate. works for treatments, not for SLC
treatment_alphas_ss <- df %>%
  nest(-treatment) %>%
  mutate(
    fit = map(data, ~nls(PercMassRemaining ~ SSasymp(fake_days, yf, y0, log_alpha), control = list(maxiter = 500), data = . )),
              tidied = map(fit, tidy),
              Augmented = map(fit, augment),)

treatment_alphas_ss_tbl <- treatment_alphas_ss %>% 
  unnest(tidied) %>% 
  select(treatment, term, estimate, std.error) %>% 
  spread(term, estimate) %>%
  mutate(k = exp(log_alpha))%>%
  filter (!is.na(k))
treatment_alphas_ss_tbl 



#trying to iterate model for SLC, but it only works for site now?
site_alphas_ss <- df %>%
  nest(-site) %>%
  mutate(
    fit = map(data, ~nls(PercMassRemaining ~ SSasymp(days_to_collection, yf, y0, log_alpha), control = list(maxiter = 500), data = . )),
              tidied = map(fit, tidy),
              Augmented = map(fit, augment),)

site_alphas_ss_tbl <- site_alphas_ss %>% 
  unnest(tidied) %>% 
  select(site, term, estimate) %>% 
  spread(term, estimate)

site_alphas_ss_tbl

#joining k values to df so that we can plot against all variables
df <- merge(df, treatment_alphas_ss_tbl, by = "treatment")
df$trtmt_k = df$k
df$trtmt_log_alph = df$
df<- mutate(df, k = NULL)

df <- merge(df, SLC_alphas_tbl, by = "SLC")
df$SLC_alpha = df$a
df<- mutate(df, a = NULL)
  
#graphs of k values

treatment_plot <- ggplot(data = df, aes(x = treatment, y = k)) + theme(axis.text.x = element_text(angle = 90)) + geom_point()
treatment_plot

       
SLC_plot <- ggplot(data = df, aes(x = SLC, y = SLC_alpha)) + theme(axis.text.x = element_text(angle = 90)) + geom_point(aes(colour = treatment))

SLC_plot

#plotting the curves themselves
  
  #the following uses the code from Douglas watson for graphing multiple curves from an interated model, but because our data is set up in this long form with many values for each treatment at each time point, it's not making any sense to look at. I think what we need here is to be seeing the lines for each SLC, not each treatment? because that's the only thing that can actually be tracked with a line over time.
augmented <- treatment_alphas_ss %>% 
  unnest(Augmented)

qplot(df$days_to_collection, df$PercMassRemaining, data = augmented, geom = 'point', colour = treatment) 
+ 
  geom_line(aes(y = .fitted))
  
  #brute forcing treatment curves, a la Alex's advice, using functions written in util.R
head(mor_eq(fake_days))

fake_days <- seq(1:345)

plot(x = fake_days, y = mor_eq(fake_days), main = "Morella brute force")

plot(x = fake_days, y = pin_eq(fake_days), main = "Pine brute force")

plot(x = fake_days, y = phrag_eq(fake_days), main = "Phrag brute force")

df%>% 
  add_predictions(treatment_alphas_ss)%>%
  ggplot(aes(x = days_to_collection, y = PercMassRemaining, group = treatment)) + 
  geom_point() + 
  geom_line(aes(x=days_to_collection, y=pred))
#this tries to create graphs from not a single model but the iterated versions, but it doesn't know how to predict from a model with so many nested layers, and I don't blame it. Help!
