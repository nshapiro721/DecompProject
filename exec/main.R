# Decomposition Data Analysis Conducted in 
#   Summer 2023 by Gedan Lab REU Noa Shapiro-Tamir
#   with assistance from Riley Leff

# Contact Noa at nshapiro@oberlin.edu or 607-351-1123 with questions

#loading libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(purrr)
library(modelr)
library(stringr)

source("exec/util.R") #a few formulas written by Riley to help obtain alpha values
source("exec/read_data.R") #reading in and cleaning data

# A few naming notes:
#   'alpha', 'a', and 'k' are all interchangeable names for decay coefficient
#   'treatment' is shorthand for decay environment
#   'SLC' is shorthand for site-litter combo, ie. a unique code for each litter type at each #      site, of which there are 27. 1 of each collected at each collection time.


# From Riley: example of using lapply() to avoid repetitive code, which creates 
#   an NLS model for each decay environment

model_formula <- as.formula(PercMassRemaining ~ exp(-a * years_to_collection))
treatments <- split(df, df$treatment)
models <- lapply(
  X = treatments, 
  FUN = function(treat_data) {
    nls(
      formula = model_formula,
      data = treat_data,
      start = list(a = 0.02)
    )
  }
) # models is a list containing a model for each treatment, 
  # using the formula defined by model_formula

# tests of the model for each treatment
deviance(models$Morella)
deviance(models$Phragmites)
deviance(models$Pine)

# Iterating treatment model for all treatments (this does probably the same thing as Riley accomplished above, but is what I did originally, as instructed by the Douglas Watson article on non-linear modeling of decay: https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/ )

treatment_alphas <- df %>%
  nest(-treatment) %>%
  mutate(
    fit = map(data,
              ~ nls(PercMassRemaining ~ 1 * exp(-a * years_to_collection), 
                    data = ., 
                    start = list(a = 0.1)
                    )
              ),
    tidied = map(fit, tidy),
    Augmented = map(fit, augment),
  )

treatment_alphas_tbl <- treatment_alphas %>%
  unnest(tidied) %>%
  select(treatment, term, estimate, std.error) %>%
  spread(term, estimate)

treatment_alphas_tbl

# Iterating models of alpha for all SLC's
SLC_alphas <- df %>%
  nest(-SLC) %>%
  mutate(
    fit = map(data, 
              ~ nls(PercMassRemaining ~ 1 * exp(-a * years_to_collection), 
                    data = ., 
                    start = list(a = 0.01)
                    )
              ),
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

SLC_alphas_tbl <- as.data.frame(SLC_alphas_tbl)
SLC_alphas_tbl$site <- substr(SLC_alphas_tbl$SLC, 1, 3)
SLC_alphas_tbl$treatment <- substr(SLC_alphas_tbl$SLC, 1, 2)
SLC_alphas_tbl$litter <- rep(c("Morella", "Phragmites", "Pinus"), 9)
SLC_alphas_tbl$litter <- as.factor(SLC_alphas_tbl$litter)

SLC_alphas_sum <- SLC_alphas_tbl %>%
  group_by(treatment, litter) %>%
  summarise(
    mean_a = mean(a),
    std.error = sd(a) / sqrt(length(a))
  )

# Graphs of alpha values: 

# graph of treatment alphas using averaged SLC data
SLC_alphas_tbl %>%
  group_by(treatment) %>%
  summarize(
    mean_alpha = mean(a),
    std.error = sd(a) / sqrt(length(a))
  ) %>%
  ggplot(aes(x = treatment, y = mean_alpha, col = treatment)) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes
    (
      ymin = mean_alpha - std.error,
      ymax = mean_alpha + std.error
    ),
    width = 0.1
  ) +
  ggtitle("Decay Coefficient (k) for each Decay Environment",
    subtitle = "(with standard error)"
  ) +
  xlab("Decay Environment") +
  ylab("k") +
  theme(legend.position = "none") +
  ylim(c(0.19, 0.29)) +
  scale_x_discrete(labels = c("Morella", "Phragmites", "Pine"))

# Graph of litter alphas using averaged SLC data
SLC_alphas_tbl %>%
  group_by(litter) %>%
  summarize(
    mean_alpha = mean(a),
    std.error = sd(a) / sqrt(length(a))
  ) %>%
  ggplot(aes(x = litter, y = mean_alpha, col = litter)) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(
      ymin = mean_alpha - std.error,
      ymax = mean_alpha + std.error
    ),
    width = 0.1
  ) +
  ggtitle("Decay Coefficient (k) for each Litter Type", subtitle = "(with standard error)") +
  xlab("Litter Type") +
  ylab("k") +
  theme(legend.position = "none")


# interaction graph
ggplot(SLC_alphas_sum, aes(x = treatment, y = mean_a, group = litter, color = litter)) +
  geom_point(position = position_dodge(width = 0.2), size = 1.5) +
  geom_errorbar(aes(ymin = mean_a - std.error, ymax = mean_a + std.error), width = 0.35, position = position_dodge(width = 0.2), linetype = 5) +
  geom_line(position = position_dodge(width = 0.2), linetype = 1) +
  ggtitle("Impact of Decay Environment/Litter Interaction\non Decay Rate (k) ") +
  xlab("Decay Environment") +
  ylab("k") +
  scale_color_discrete(name = "Litter") +
  theme(
    panel.grid.major = element_blank())

# anova & tukey tests of site/litter interactions
SLC_anova <- aov(data = SLC_alphas_tbl, a ~ treatment * litter)
summary(SLC_anova)
TukeyHSD(SLC_anova, "treatment")


# Plotting the curves:
df <- df %>% mutate(ID = row_number())

morella_preds <- df %>%
  filter(treatment == "Morella") %>%
  add_predictions(models$Morella) %>%
  select(pred, ID)

phrag_preds <- df %>%
  filter(treatment == "Phragmites") %>%
  add_predictions(models$Phragmites) %>%
  select(pred, ID)

pine_preds <- df %>%
  filter(treatment == "Pine") %>%
  add_predictions(models$Pine) %>%
  select(pred, ID)

preds <- rbind(morella_preds, phrag_preds, pine_preds)

df <- merge(preds, df, by = "ID")

ggplot(
  data = df,
  aes(x = years_to_collection, y = PercMassRemaining)
) +
  geom_point(col = "dark gray") +
  geom_smooth(
    aes(
      y = pred,
      group = treatment,
      col = treatment,
      fill = treatment
    ),
    alpha = 0.3
  ) +
  ggtitle("Modeled Decay Rate Curves for each Decay Environment") +
  labs(subtitle = "    with 95% confidence interval") +
  xlab("Years to Collection") +
  xlim(c(0, 1)) +
  ylab("Percent Mass Remaining") +
  scale_color_discrete(name = "Decay\nEnvironment") +
  scale_fill_discrete(name = "Decay\nEnvironment")

# some tests to make sure the curves are actually curves
four_years <- as.data.frame(seq(1, 4, 0.02)) %>%
  rename(four_years_continuous = seq)

ten_years <- as.data.frame(seq(1, 10, 0.02))

perc_mass_to_eyeball_fit_ten <- ten_years %>%
  rename("years_to_collection" = "seq(1, 10, 0.02)") %>%
  mutate(Morella = 
           unlist(get_mass_pct_remaining_at_t(
                     alpha = coef(models$Morella)[["a"]],
                     t = ten_years)
                  ),
         Phragmites = 
             unlist(get_mass_pct_remaining_at_t(
               alpha = coef(models$Phragmites)[["a"]],
               t = ten_years)
                    ),
         Pine = 
           unlist(get_mass_pct_remaining_at_t(
             alpha = coef(models$Pine)[["a"]],
             t = ten_years)
                  )
         ) %>%
  pivot_longer(cols = c("Morella", "Phragmites", "Pine"),
               names_to = "treatment",
               values_to = "pmr")

# Graph showing decay curves for each environment over 10 years
ggplot(data = perc_mass_to_eyeball_fit_ten, aes(x = years_to_collection)) +
  geom_smooth(aes(y = pmr,
                  group = treatment,
                  col = treatment)) +
  geom_smooth(
    data = df, 
    aes(y = pred, col = treatment), se = FALSE) +
  geom_point(
    data = df,
    aes(x = years_to_collection, y = PercMassRemaining)
  ) +
  ggtitle("Decay Curves of each Decay Environment Over 10 Years") +
  ylab("Percent Mass Remaining") +
  xlab("Years") +
  scale_color_discrete(labels = c("Morella", "Phragmites", "Pine"),
                       name = "Decay\nEnvironment") +
theme(legend.position = c(0.85,0.7))


# Merging in the soil data, averaged for each site
df <- df %>%
  merge(soils_decomp_clean_site, by = "site")

# Looking at patterns of salinity and moisture for each treatment group

#  treatment by moisture scatterplot (with error bars)
ggplot(soils_decomp_clean_treatment, aes(y = mean_moisture, col = treatment)) +
  geom_errorbar(aes(x = treatment, ymin = mean_moisture - std.error_mois, ymax = mean_moisture + std.error_mois), width = 0.15) +
  geom_point(aes(x = treatment), size = 2.3) +
  ggtitle("Average Soil Moisture of Each Decay Environment") +
  ylab("Average Soil Moisture") +
  xlab("Decay Environment") +
  scale_x_discrete(labels = c("Morella", "Phragmites", "Pine")) +
  theme(legend.position = "none")

#  treatment by salinity scatterplot (with error bars)
ggplot(soils_decomp_clean_treatment, aes(y = mean_salinity)) +
  geom_errorbar(aes(x = treatment, ymin = mean_salinity - std.error_sal, ymax = mean_salinity + std.error_sal, col = treatment), width = 0.15) +
  geom_point(aes(x = treatment, fill = treatment, col = treatment), size = 2.3) +
  ggtitle("Average Soil Salinity of Each Decay Environment") +
  ylab("Average Soil Salinity") +
  xlab("Decay Environment") +
  scale_x_discrete(labels = c("Morella", "Phragmites", "Pine")) +
  theme(legend.position = "none")

#  regressions to find statistical significance of the soil salinity and moisture impacts on  site decay rate

df_site_regressions <- df %>%
  group_by(site) %>%
  summarize(
    mean_alpha = mean(a),
    mean_salinity = mean(mean_salinity),
    mean_moisture = mean(mean_moisture)
  ) %>%
  mutate(treatment = substr(site, 1, 2))

site_regression_sal <- df_site_regressions %>%
  lm(formula = mean_salinity ~ mean_alpha)
summary(site_regression_sal)

site_regression_mois <- df_site_regressions %>%
  lm(formula = mean_moisture ~ mean_alpha)
summary(site_regression_mois)


multivariate_site <- lm(df_site_regressions, formula = mean_alpha ~ mean_salinity * mean_moisture)
summary(multivariate_site)

# Graphing the impacts/lack thereof

# Avg site decay rate by site soil salinity
ggplot(data = df_site_regressions, aes(x = mean_salinity, y = mean_alpha)) +
  geom_point(aes(col = treatment), size = 2.3) +
  # geom_smooth(method = "lm") +
  ggtitle("Site Soil Salinity as Predictor of Site Average Decay Rate") +
  xlab("Site Soil Salinity") +
  ylab("Site Average Decay Rate") +
  annotate(
    geom = "text",
    label = "P-value = 0.30",
    fontface = 2,
    x = 0.7,
    y = 0.16
  ) +
  annotate(
    geom = "text",
    label = "R^2 = 0.03",
    fontface = 2,
    x = 0.7,
    y = 0.17
  ) +
  scale_color_discrete(
    name = "Decay\nEnvironment",
    labels = c("Morella", "Phragmites", "Pine")
  ) +
  ylim(c(0.15, 0.29))

# Avg site decay rate by site soil moisture
ggplot(data = df_site_regressions, aes(x = mean_moisture, y = mean_alpha)) +
  geom_point(aes(col = treatment), size = 2.3) +
  # geom_smooth(method = "lm") +
  ggtitle("Site Soil Moisture as Predictor of Site Average Decay Rate") +
  xlab("Site Soil Moisture") +
  ylab("Site Average Decay Rate") +
  annotate(
    geom = "text",
    label = "P-value = 0.201",
    fontface = 2,
    x = 0.7,
    y = 0.15
  ) +
  annotate(
    geom = "text",
    label = "R^2 = 0.1099",
    fontface = 2,
    x = 0.7,
    y = 0.16
  ) +
  scale_color_discrete(
    name = "Decay\nEnvironment",
    labels = c("Morella", "Phragmites", "Pine")
  )