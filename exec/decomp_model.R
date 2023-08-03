library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(purrr)
library(modelr)
library(stringr)
source("exec/util.R")

# Importing data
df <- read.csv("data/decomp_data.csv")
sitedf <- read.csv("data/site_contents.csv")[, c(1, 4)]
initials <- read.csv("data/initials.csv")
df <- merge(df, sitedf, by = "tag")
df$SLC <- interaction(as.factor(df$site), as.factor(df$class))
df$PercMassRemaining <- 1 - (df$init_total_mass - df$post_total_mass) / df$init_mass_litter
df$post_litter_mass <- df$init_mass_litter - df$mass_loss
df <- bind_rows(df, initials)
df[117, 3] <- "Phragmites"
# Changing the days_to_collection values to reflect accurate collection dates
df <- df %>%
  mutate(days_to_collection = if_else(days_to_collection == 28, 22, days_to_collection)) %>%
  mutate(days_to_collection = if_else(days_to_collection == 84, 76, days_to_collection)) %>%
  mutate(days_to_collection = if_else(days_to_collection == 182, 163, days_to_collection)) %>%
  mutate(days_to_collection = if_else(days_to_collection == 365, 345, days_to_collection))

df$years_to_collection <- df$days_to_collection/365

# visualizing the weird bump at the third collection date
ggplot(df, aes(x = days_to_collection, y = PercMassRemaining)) +
  geom_point()
boxplot(df$PercMassRemaining ~ df$days_to_collection, outlier_tagging = TRUE)

find_outliers_df <- df %>%
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
  post_litter_mass ~ init_mass_litter * exp(-a * years_to_collection),
  data = df %>% filter(treatment == "Morella"),
  start = list(a = 0.02)
)
cool_model

# here's a sanity check for the estimate of alpha that it spits out
# you'll need to do the actual stats on it but here's a gander
# get_mass_pct_remaining_at_t is also a function from "exec/util.R"
t_to_eyeball_fit <- 1:365
perc_mass_to_eyeball_fit <- get_mass_pct_remaining_at_t(
  a = coef(cool_model)[["a"]],
  t = t_to_eyeball_fit
)
plot(
  x = df[which(df$treatment == "Morella"), "days_to_collection"],
  y = df[which(df$treatment == "Morella"), "PercMassRemaining"]
)
lines(
  x = t_to_eyeball_fit,
  y = perc_mass_to_eyeball_fit,
  t = "p",
  col = "blue"
)

# Noa makes the same model but with percents. Has a lower residual sum of squares so kept it. Want to double check the
noa_model <- nls(
  PercMassRemaining ~ 1 * exp(-a * years_to_collection),
  data = df %>% filter(treatment == "Morella"),
  start = list(a = 0.02)
)

noa_model


# Testing Noa model - trying to do here what Riley did with Cool model. Not sure if it actually shows us new info or not.
t_to_eyeball_fit <- 1:365
perc_mass_to_eyeball_fit_noa <- get_mass_pct_remaining_at_t(
  a = coef(noa_model)[["a"]],
  t = t_to_eyeball_fit
)
plot(
  x = df[which(df$treatment == "Morella"), "years_to_collection"],
  y = df[which(df$treatment == "Morella"), "PercMassRemaining"]
)
lines(
  x = t_to_eyeball_fit,
  y = perc_mass_to_eyeball_fit_noa,
  t = "p",
  col = "blue"
)

  #trying to plot a singular noa_model with ggplot and geom_smooth... not working yet
ggplot(data=df %>% filter(treatment == "Morella"), 
       aes(x = years_to_collection, 
           y = PercMassRemaining)) + 
  geom_point() +
  stat_smooth(method="nls", 
              formula= PercMassRemaining ~ 1 * exp(-a * years_to_collection),
              method.args = li(
                start = list(a = 0.02, PercMassRemaining = 1, years_to_collection = 0)
              ))

noa_model

# Iterating noa_model for all treatments
treatment_alphas <- df %>%
  nest(-treatment) %>%
  mutate(
    fit = map(data, ~ nls(PercMassRemaining ~ 1 * exp(-a * years_to_collection), data = ., start = list(a = 0.1))),
    tidied = map(fit, tidy),
    Augmented = map(fit, augment),
  )

treatment_alphas_tbl <- treatment_alphas %>%
  unnest(tidied) %>%
  select(treatment, term, estimate, std.error) %>%
  spread(term, estimate)

treatment_alphas_tbl

#graphing treatment alphas with standard error


# Iterating noa_model for all Site/Treatment combos
SLC_alphas <- df %>%
  nest(-SLC) %>%
  mutate(
    fit = map(data, ~ nls(PercMassRemaining ~ 1 * exp(-a * years_to_collection), data = ., start = list(a = 0.01))),
    tidied = map(fit, tidy),
    Augmented = map(fit, augment),
  )

SLC_alphas_tbl <-
  SLC_alphas %>%
  unnest(tidied) %>%
  select(SLC, term, estimate, std.error) %>%
  spread(term, estimate)


SLC_alphas_tbl

df <- df %>%
  merge(SLC_alphas_tbl, by = "SLC")

  #graphing SLC alpha values



# graphs of k values
    #before joining

ggplot(treatment_alphas_tbl, aes(x = treatment, y = a)) +
  geom_bar(stat = "identity", width = 0.5, fill = "light blue") +
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error), width = 0.2)

ggplot(SLC_alphas_tbl, aes(x = reorder(SLC, -a), y = a)) +
  geom_bar(stat = "identity", width = 0.5, aes(fill = litter)) +
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error), width = 0.2) + 
  theme(axis.text.x = element_text(angle = 90))

SLC_alphas_tbl <- as.data.frame(SLC_alphas_tbl) 
SLC_alphas_tbl$site <- substr(SLC_alphas_tbl$SLC, 1, 3) 
SLC_alphas_tbl$treatment <- substr(SLC_alphas_tbl$SLC, 1, 2)
SLC_alphas_tbl$litter = rep(c("Morella", "Phragmites", "Pinus"), 9)
SLC_alphas_tbl$litter = as.factor(SLC_alphas_tbl$litter)

ggplot(SLC_alphas_tbl, aes(x=litter, y=a, group = treatment))+
  geom_point(aes(color=treatment))+
  geom_line(aes(color=treatment))+
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error), width = 0.2)
# each site as a different point

SLC_alphas_sum = SLC_alphas_tbl%>% group_by(treatment, litter) %>% 
  summarise(mean_a = mean(a),
            std.error = sd(a)/sqrt(length(a)))

ggplot(SLC_alphas_sum, aes(x=treatment, y=mean_a, group = litter, color=litter))+
  geom_point(position=position_dodge(width=0.2))+
  geom_errorbar(aes(ymin = mean_a - std.error, ymax = mean_a + std.error), width = 0.35, position=position_dodge(width=0.2)) + 
  geom_line(position = position_dodge(width = 0.2))

SLC_anova = aov(data=SLC_alphas_tbl, a ~ treatment*litter)
summary(SLC_anova)
TukeyHSD(SLC_anova, "treatment")

# plotting the curves themselves
df %>%
  filter(SLC == "Ph2.Pine") %>%
  add_predictions(noa_model) %>%
  ggplot(aes(x = days_to_collection, y = PercMassRemaining, group = treatment)) +
  geom_point() +
  geom_line(aes(x = days_to_collection, y = pred)) +
  ggtitle("Decay Curve: Morella Treatment") 

df %>%
  filter(SLC == "Ph2.Pine") %>%
  add_predictions(noa_model) %>%
  ggplot(aes(x = days_to_collection, y = PercMassRemaining, group = treatment)) +
  geom_point() +
  geom_smooth(method = "nls", 
              method.args = list(formula= PercMassRemaining ~ 1 * exp(-a * years_to_collection),
                                 start = list(PercMassRemaining = 1, a = 0.02, years_to_collection = 0)
                                 ))
  
  ggtitle("Decay Curve: Morella Treatment") 

#Bringing in soil data
soils_decomp <- read.csv("data/Decomp_soils.csv")
soils_decomp[21, 2] <- "Pi1"
soils_decomp_clean <- 
  soils_decomp %>% 
    group_by (site) %>%
    summarise(mean_moisture = mean(percentage_moisture), 
              mean_EC = mean(EC_uS.cm), 
              mean_salinity = mean(Salinity_ppt))
df <- df %>%
  merge(soils_decomp_clean, by = "site")

#making regressions of soils + decomp data

df_phrag_regression <- df %>% 
  filter(class == "Phragmites")

    phrag_regression_sal <- df_phrag_regression %>%
      lm(formula = mean_salinity ~ a)
    summary(phrag_regression_sal)
  
    phrag_regression_mois <- df_phrag_regression %>%
      lm(formula = mean_moisture ~ a)
    summary(phrag_regression_mois)

df_mor_regression <- df %>%
  filter(class == "Morella")

    mor_regression_sal <- df_mor_regression %>%
     lm(formula = mean_salinity ~ a)
    summary(mor_regression_sal)
    
    mor_regression_mois <- df_mor_regression %>%
      lm(formula = mean_moisture ~ a)
    summary(mor_regression_mois)

df_pine_regression <- df %>%
  filter(class == "Pinus")

    pine_regression_sal <- df_pine_regression %>%
     lm(formula = mean_salinity ~ a)
    summary(pine_regression_sal)

    pine_regression_mois <- df_pine_regression %>%
      lm(formula = mean_moisture ~ a)
    summary(pine_regression_mois)

ggplot(data= df, aes(x = mean_salinity, y = a)) + 
  geom_point(aes(col = class)) +
  geom_smooth(data = df_phrag_regression, method = "lm", col = "#00BA38") +
  geom_smooth(data = df_mor_regression, method = "lm", col = "#F8766D") +
  geom_smooth(data = df_pine_regression, method = "lm", col = "#619CFF")

ggplot(data= df, aes(x = mean_moisture, y = a)) + 
   geom_point(aes(col = class)) +
   geom_smooth(data = df_phrag_regression, method = "lm", col = "#00BA38") +
   geom_smooth(data = df_mor_regression, method = "lm", col = "#F8766D") +
   geom_smooth(data = df_pine_regression, method = "lm", col = "#619CFF")

soil_anova <- aov(data=df, a ~ mean_salinity*mean_moisture)
summary(soil_anova)

