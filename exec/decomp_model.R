library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(purrr)
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
#Changing the days_to_collection values to reflect accurate collection dates
df <- df %>%
  mutate(days_to_collection = if_else(days_to_collection == 28, 22, days_to_collection))%>%
  mutate(days_to_collection = if_else(days_to_collection == 84, 76, days_to_collection))%>%
  mutate(days_to_collection = if_else(days_to_collection == 182, 163, days_to_collection))%>%
  mutate(days_to_collection = if_else(days_to_collection == 365, 345, days_to_collection))
         
#visualizing the weird bump at the third collection date
ggplot(df, aes(x = days_to_collection, y = PercMassRemaining)) + geom_point()
boxplot(df$PercMassRemaining ~ df$days_to_collection)


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

#trying to do self-starting model... IT WORKS NOW???
fit <- nls(PercMassRemaining ~ SSasymp(days_to_collection, yf, y0, log_alpha), data = df %>% filter(treatment == "Morella"))

fit

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

#Iterating noa_model for all treatments 
treatment_alphas <- df %>% 
  nest(-treatment) %>%
  mutate(
    fit = map(data, ~nls(PercMassRemaining ~ 1* exp(-a * days_to_collection), data = ., start = list(a = 0.002))),
    tidied = map(fit, tidy),
    Augmented = map(fit, augment),)

treatment_alphas_tbl <- treatment_alphas %>% 
  unnest(tidied) %>% 
  select(treatment, term, estimate) %>% 
  spread(term, estimate)

treatment_alphas_tbl

#Iterating noa_model for all Site/Treatment combos
SLC_alphas <- df %>% 
  nest(-SLC) %>%
  mutate(
    fit = map(data, ~nls(PercMassRemaining ~ 1* exp(-a * days_to_collection), data = ., start = list(a = 0.002))),
    tidied = map(fit, tidy),
    Augmented = map(fit, augment),)

SLC_alphas
SLC_alphas_tbl <- 
  SLC_alphas %>% 
  unnest(tidied) %>% 
  select(SLC, term, estimate) %>% 
  spread(term, estimate)

SLC_alphas_tbl

#trying to get the self-starter to iterate. Not working yet.
treatment_alphas_ss <- df %>%
  nest(-treatment) %>%
  mutate(
    fit = map(data, ~nls(PercMassRemaining ~ SSasymp(days_to_collection, g, pl, log_alpha), data = df %>% filter(treatment == "Morella")),
              tidied = map(fit, tidy),
              Augmented = map(fit, augment)),)

treatment_alphas_ss_tbl <- treatment_alphas_ss %>% 
  unnest(tidied) %>% 
  select(treatment, term, estimate) %>% 
  spread(term, estimate)
treatment_alphas_ss_tbl

#joining alpha values to df so that we can plot against all variables
df <- merge(df, treatment_alphas_tbl, by = "treatment")
df$treatment_alpha = df$a
df<- mutate(df, a = NULL)

df <- merge(df, SLC_alphas_tbl, by = "SLC")
df$SLC_alpha = df$a
df<- mutate(df, a = NULL)
  
#graphs of alpha values

treatment_plot <- ggplot(data = df, aes(x = treatment, y = treatment_alpha)) + theme(axis.text.x = element_text(angle = 90)) + geom_point()
treatment_plot
       
SLC_plot <- ggplot(data = df, aes(x = SLC, y = SLC_alpha)) + theme(axis.text.x = element_text(angle = 90)) + geom_point(aes(colour = treatment))

SLC_plot

