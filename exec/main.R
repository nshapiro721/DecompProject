library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(purrr)
library(modelr)
library(stringr)

source("exec/util.R")
source("exec/read_data.R")

# From Riley: example of using lapply() to avoid repetitive code.
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

# Testing Noa model - trying to do here what Riley did with Cool model. Not sure if it actually shows us new info or not.
t_to_eyeball_fit <- 1:365
# perc_mass_to_eyeball_fit_noa <- get_mass_pct_remaining_at_t(
#   a = coef(noa_model)[["a"]],
#   t = t_to_eyeball_fit
# )
# plot(
#   x = df[which(df$treatment == "Morella"), "years_to_collection"],
#   y = df[which(df$treatment == "Morella"), "PercMassRemaining"]
# )
# lines(
#   x = t_to_eyeball_fit,
#   y = perc_mass_to_eyeball_fit_noa,
#   t = "p",
#   col = "blue"
# )

# Iterating noa_model (now models$Morella) for all treatments
treatment_alphas <- df %>%
  nest(-treatment) %>%
  mutate(
    fit = map(data, ~ nls(PercMassRemaining ~ 1 * exp(-a * years_to_collection), data = df, start = list(a = 0.1))),
    tidied = map(fit, tidy),
    Augmented = map(fit, augment),
  )

treatment_alphas_tbl <- treatment_alphas %>%
  unnest(tidied) %>%
  select(treatment, term, estimate, std.error) %>%
  spread(term, estimate)

treatment_alphas_tbl

deviance(models$Morella)
deviance(models$Phragmites)
deviance(models$Pine)

# graphing treatment alphas with standard error from treatment model
# Bar version
ggplot(treatment_alphas_tbl, aes(x = treatment, y = a)) +
  geom_bar(stat = "identity", width = 0.5, fill = "light blue") +
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error), width = 0.2)

# Point version (preferred)
ggplot(treatment_alphas_tbl, aes(x = treatment, y = a)) +
  geom_point(aes(col = treatment), size = 2) +
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error, col = treatment), width = 0.1) +
  ggtitle("Decay Coefficient (k) for each Decay Environment \n (with standard error)") +
  xlab("Decay Environment") +
  ylab("k") +
  theme(legend.position = "none") +
  ylim(c(0.19, 0.29))

# Iterating noa_model (now models$Morella) for all Site/Treatment combos
SLC_alphas <- df %>%
  nest(-SLC) %>%
  mutate(
    fit = map(data, ~ nls(PercMassRemaining ~ 1 * exp(-a * years_to_collection), data = df, start = list(a = 0.01))),
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

# graphs of k values
# before joining

SLC_alphas_tbl <- as.data.frame(SLC_alphas_tbl)
SLC_alphas_tbl$site <- substr(SLC_alphas_tbl$SLC, 1, 3)
SLC_alphas_tbl$treatment <- substr(SLC_alphas_tbl$SLC, 1, 2)
SLC_alphas_tbl$litter <- rep(c("Morella", "Phragmites", "Pinus"), 9)
SLC_alphas_tbl$litter <- as.factor(SLC_alphas_tbl$litter)

ggplot(SLC_alphas_tbl, aes(x = reorder(SLC, -a), y = a)) +
  geom_point(aes(col = treatment), size = 2) +
  geom_errorbar(
    aes(
      ymin = a - std.error, ymax = a + std.error,
      col = treatment
    ),
    width = 0.08
  ) +
  scale_color_discrete(name = "Decay\nEnvironment") +
  theme(
    axis.text.x = element_text(angle = 90)
    #  , legend.title = element_text("Decay Environment")
  ) +
  ggtitle("Decay Coefficient (k) for each Site/Litter Combination (SLC)") +
  xlab("SLC") +
  ylab("k")



ggplot(SLC_alphas_tbl, aes(x = treatment, y = a, group = treatment)) +
  geom_point(aes(color = litter))
# geom_line(aes(color=treatment))+
# geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error), width = 0.2)
# ^each site as a different point

SLC_alphas_sum <- SLC_alphas_tbl %>%
  group_by(treatment, litter) %>%
  summarise(
    mean_a = mean(a),
    std.error = sd(a) / sqrt(length(a))
  )

# Graph of treatment alphas using averaged SLC data
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
    panel.grid.major = element_blank()
    #      ,
    #    panel.grid.minor = element_blank()
  )

# bar version, which I like less
ggplot(SLC_alphas_sum, aes(x = treatment, y = mean_a, group = litter, fill = litter)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  geom_errorbar(aes(ymin = mean_a - std.error, ymax = mean_a + std.error),
    width = 0.2,
    position = position_dodge(width = 0.6)
  )

SLC_anova <- aov(data = SLC_alphas_tbl, a ~ treatment * litter)
summary(SLC_anova)
TukeyHSD(SLC_anova, "treatment")

# plotting the curves themselves



# first, the real thing

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

# then, some tests...
four_years <- as.data.frame(seq(1, 4, 0.02)) %>%
  rename(four_years_continuous = seq)

ten_years <- as.data.frame(seq(1, 10, 0.02))
noa_model_tester <- nls(
  PercMassRemaining ~ 1 * exp(-a * years_to_collection),
  data = df %>% filter(treatment == "Morella"),
  start = list(a = 0.02)
)

noa_model_tester

t_to_eyeball_fit <- 1:1000

perc_mass_to_eyeball_fit_four <- four_years %>%
  rename("years_to_collection" = "seq(1, 4, 0.02)") %>%
  mutate(pmr = unlist(get_mass_pct_remaining_at_t(
    alpha = coef(noa_model_tester)[["a"]],
    t = four_years
  )))

perc_mass_to_eyeball_fit_ten <- ten_years %>%
  rename("years_to_collection" = "seq(1, 10, 0.02)") %>%
  mutate(pmr = unlist(get_mass_pct_remaining_at_t(
    alpha = coef(noa_model_tester)[["a"]],
    t = ten_years
  )))


perc_mass_to_eyeball_fit

# OFFICIAL Graph showing that the curves are indeed curves over 10 years
ggplot(data = perc_mass_to_eyeball_fit_ten, aes(x = years_to_collection)) +
  geom_smooth(aes(y = pmr),
    color = "salmon"
  ) +
  geom_smooth(data = df %>% filter(treatment == "Morella"), aes(y = pred), se = FALSE, col = "salmon") +
  geom_point(
    data = df %>%
      filter(treatment == "Morella") %>%
      add_predictions(model = models$Morella),
    aes(x = years_to_collection, y = PercMassRemaining)
  ) +
  ggtitle("Decay Curve of Litter Under Morella Over 10 Years") +
  ylab("Percent Mass Remaining") +
  xlab("Years")

# Bringing in soil data
soils_decomp <- read.csv("data/Decomp_soils.csv")
soils_decomp[21, 2] <- "Pi1"
soils_decomp$treatment <- substr(soils_decomp$site, 1, 2)

soils_decomp_clean_site <-
  soils_decomp %>%
  group_by(site) %>%
  summarise(
    mean_moisture = mean(percentage_moisture),
    std.error_mois = sd(percentage_moisture) / sqrt(length(percentage_moisture)),
    mean_EC = mean(EC_uS.cm),
    std.error_EC = sd(EC_uS.cm) / sqrt(length(EC_uS.cm)),
    mean_salinity = mean(Salinity_ppt),
    std.error_sal = sd(Salinity_ppt) / sqrt(length(Salinity_ppt))
  ) %>%
  mutate(
    site = as.factor(site),
    treatment = substr(site, 1, 2)
  )


soils_decomp_clean_treatment <-
  soils_decomp %>%
  group_by(treatment) %>%
  summarise(
    mean_moisture = mean(percentage_moisture),
    std.error_mois = sd(percentage_moisture) / sqrt(length(percentage_moisture)),
    mean_EC = mean(EC_uS.cm),
    std.error_EC = sd(EC_uS.cm) / sqrt(length(EC_uS.cm)),
    mean_salinity = mean(Salinity_ppt),
    std.error_sal = sd(Salinity_ppt) / sqrt(length(Salinity_ppt))
  ) %>%
  mutate(site = as.factor(treatment))

df <- df %>%
  merge(soils_decomp_clean_site, by = "site")

# making regressions of soils + decomp data
ggplot(soils_decomp_clean_site, aes(y = mean_salinity)) +
  geom_bar(stat = "identity", aes(x = site, fill = site)) +
  geom_errorbar(aes(x = site, ymin = mean_salinity - std.error_sal, ymax = mean_salinity + std.error_sal), width = 0.2)


# treatment by moisture barplot  (with error bars)
ggplot(soils_decomp_clean_treatment, aes(y = mean_moisture)) +
  geom_bar(stat = "identity", aes(x = treatment, fill = treatment), width = 0.4) +
  geom_errorbar(aes(x = treatment, ymin = mean_moisture - std.error_mois, ymax = mean_moisture + std.error_mois), width = 0.15)

# treatment by moisture scatterplot (with error bars) *preferred
ggplot(soils_decomp_clean_treatment, aes(y = mean_moisture, col = treatment)) +
  geom_errorbar(aes(x = treatment, ymin = mean_moisture - std.error_mois, ymax = mean_moisture + std.error_mois), width = 0.15) +
  geom_point(aes(x = treatment), size = 2.3) +
  ggtitle("Average Soil Moisture of Each Decay Environment") +
  ylab("Average Soil Moisture") +
  xlab("Decay Environment") +
  scale_x_discrete(labels = c("Morella", "Phragmites", "Pine")) +
  theme(legend.position = "none")

# treatment by salinity barplot  (with error bars)
ggplot(soils_decomp_clean_treatment, aes(y = mean_salinity)) +
  geom_bar(stat = "identity", aes(x = treatment, fill = treatment), width = 0.4) +
  geom_errorbar(aes(x = treatment, ymin = mean_salinity - std.error_sal, ymax = mean_salinity + std.error_sal), width = 0.15)

# treatment by salinity scatterplot (with error bars)
ggplot(soils_decomp_clean_treatment, aes(y = mean_salinity)) +
  geom_errorbar(aes(x = treatment, ymin = mean_salinity - std.error_sal, ymax = mean_salinity + std.error_sal, col = treatment), width = 0.15) +
  geom_point(aes(x = treatment, fill = treatment, col = treatment), size = 2.3) +
  ggtitle("Average Soil Salinity of Each Decay Environment") +
  ylab("Average Soil Salinity") +
  xlab("Decay Environment") +
  scale_x_discrete(labels = c("Morella", "Phragmites", "Pine")) +
  theme(legend.position = "none")

# new ones by site

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
  geom_point(size = 2.3) +
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

# showing how the low moisture outlier skews the whole thing terribly
soils_decomp_clean_site %>%
  filter(mean_moisture > 0.6) %>%
  ggplot(aes(x = mean_moisture, y = mean_salinity)) +
  geom_point(aes(col = treatment)) +
  geom_smooth(method = "lm")
