library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(purrr)
library(modelr)
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
  geom_smooth(method="nls", 
              method.args = list(formula= PercMassRemaining ~ 1 * exp(-a * years_to_collection), 
              start=0.02),
              data = df %>% filter(treatment == "Morella"),
              se = FALSE)

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

  #graphing SLC alpha values



# graphs of k values
    #before joining

ggplot(treatment_alphas_tbl, aes(x = treatment, y = a)) +
  geom_bar(stat = "identity", width = 0.5, fill = "light blue") +
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error), width = 0.2)

ggplot(SLC_alphas_tbl, aes(x = reorder(SLC, -a), y = a)) +
  geom_bar(stat = "identity", width = 0.5, fill = "orange") +
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error), width = 0.2) + theme(axis.text.x = element_text(angle = 90))

    #joining...
df <- df %>%
  merge(treatment_alphas_tbl, by = "treatment") %>%
  rename(trtmt_a = a,
         trtmt_std_err = std.error) %>%
  merge(SLC_alphas_tbl, by = "SLC") %>%
  rename(slc_a = a,
         slc_std_err = std.error)

ggplot(data = df, aes(x = reorder(SLC, -slc_a), y = slc_a, fill = treatment)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = slc_a - slc_std_err, ymax = slc_a + slc_std_err, width = 0.2)) +
  theme(axis.text.x = element_text(angle = 90))



treatment_plot <- ggplot(data = df, aes(x = treatment, y = trtmt_a)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_bar(stat = "identity", width = 0.5, fill = "light blue")

treatment_plot

rm(treatment_plot)

SLC_plot <- ggplot(data = df, aes(x = SLC, y = SLC_alpha)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point(aes(colour = treatment))
SLC_plot



# plotting the curves themselves

