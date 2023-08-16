# trying to do self-starting model... IT WORKS NOW!
fit_morella <- nls(PercMassRemaining ~ SSasymp(days_to_collection, 0, 1, log_alpha), data = df %>% filter(treatment == "Morella"))

fit_morella

# Plotting decay curve of non-iterated self-starting model
# this first one doesn't work yet
grid_morella <- data.frame(x = seq(0, 345, length = 346))
grid_morella %>% add_predictions(fit_morella)

plot(df$PercMassRemaining ~ df$days_to_collection)
curve(predict(fit_morella, grid_morella))

# this one should work
df %>%
  filter(treatment == "Morella") %>%
  add_predictions(fit_morella) %>%
  ggplot(aes(x = days_to_collection, y = PercMassRemaining, group = treatment)) +
  geom_point() +
  geom_line(aes(x = days_to_collection, y = pred)) +
  ggtitle("Decay Curve: Morella Treatment")

# trying to get the self-starter to iterate. works for treatments, not for SLC
treatment_alphas_ss <- df %>%
  nest(-treatment) %>%
  mutate(
    fit = map(
      data,
      ~ nls(
        PercMassRemaining ~ SSasymp(
          days_to_collection,
          yf,
          y0,
          log_alpha
        ),
        control = list(maxiter = 500), data = .
      )
    ),
    tidied = map(fit, tidy),
    Augmented = map(fit, augment)
  )

treatment_alphas_ss_tbl <- treatment_alphas_ss %>%
  unnest(tidied) %>%
  select(treatment, term, estimate, std.error) %>%
  spread(term, estimate) %>%
  mutate(k = exp(log_alpha)) %>%
  filter(!is.na(k))
treatment_alphas_ss_tbl

ggplot(treatment_alphas_ss_tbl, aes(x = treatment, y = log_alpha)) +
  geom_bar(stat = "identity", width = 0.5, fill = "light blue") +
  geom_errorbar(aes(ymin = log_alpha - std.error, ymax = log_alpha + std.error), width = 0.2)

# trying to iterate model for SLC, but it only works for site now?
site_alphas_ss <- df %>%
  nest(-SLC) %>%
  mutate(
    fit = map(
      data,
      ~ nls(PercMassRemaining ~ SSasymp(days_to_collection, yf, y0, log_alpha), control = list(maxiter = 500), data = .)
    ),
    tidied = map(fit, tidy),
    Augmented = map(fit, augment),
  )

site_alphas_ss_tbl <- site_alphas_ss %>%
  unnest(tidied) %>%
  select(site, term, estimate) %>%
  spread(term, estimate)

site_alphas_ss_tbl

# joining k values to df so that we can plot against all variables
df <- merge(df, treatment_alphas_ss_tbl, by = "treatment")
df$trtmt_k <- df$k
df$trtmt_log_alph <- df$log_alpha
df <- mutate(df, k, log_alpha, y0, yf = NULL)

df <- merge(df, SLC_alphas_tbl, by = "SLC")
df$SLC_alpha <- df$a
df <- mutate(df, a = NULL)


# the following uses the code from Douglas watson for graphing multiple curves from an interated model, but because our data is set up in this long form with many values for each treatment at each time point, it's not making any sense to look at. I think what we need here is to be seeing the lines for each SLC, not each treatment? because that's the only thing that can actually be tracked with a line over time.
augmented <- treatment_alphas %>%
  unnest(Augmented)

qplot(df$days_to_collection, df$PercMassRemaining, data = augmented, geom = "point", colour = treatment) +
  geom_line(aes(y = .fitted))

# brute forcing treatment curves, a la Alex's advice, using functions written in util.R
head(mor_eq(fake_days))

fake_days <- seq(1:345)

plot(x = fake_days, y = mor_eq(fake_days), main = "Morella brute force")

plot(x = fake_days, y = pin_eq(fake_days), main = "Pine brute force")

plot(x = fake_days, y = phrag_eq(fake_days), main = "Phrag brute force")

df %>%
  add_predictions(treatment_alphas) %>%
  ggplot(aes(x = days_to_collection, y = PercMassRemaining, group = treatment)) +
  geom_point() +
  geom_line(aes(x = days_to_collection, y = pred))
# this tries to create graphs from not a single model but the iterated versions, but it doesn't know how to predict from a model with so many nested layers, and I don't blame it. Help!


# joining...
df %>%
  merge(treatment_alphas_tbl, by = "treatment") %>%
  rename(
    trtmt_a = a,
    trtmt_std_err = std.error
  ) %>%
  merge(SLC_alphas_tbl, by = "SLC") %>%
  rename(
    slc_a = a,
    slc_std_err = std.error
  )



ggplot(data = df, aes(x = reorder(SLC, -slc_a), y = slc_a, fill = treatment)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_errorbar(aes(ymin = slc_a - slc_std_err, ymax = slc_a + slc_std_err, width = 0.2)) +
  theme(axis.text.x = element_text(angle = 90))



treatment_plot <- ggplot(data = df, aes(x = treatment, y = trtmt_a)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_bar(stat = "identity", width = 0.5, fill = "light blue")

treatment_plot

rm(treatment_plot)

SLC_plot <- ggplot(data = df, aes(x = SLC, y = SLC_a)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point(aes(colour = treatment))
SLC_plot

# OG version of salinity regression plot before taking out phrag and morella
ggplot(data = df %>% filter(!is.na(class)), aes(x = mean_salinity, y = a)) +
  geom_point(aes(col = class)) +
  geom_smooth(data = df_phrag_regression, method = "lm", col = "#00BA38") +
  geom_smooth(data = df_mor_regression, method = "lm", col = "#F8766D") +
  geom_smooth(data = df_pine_regression, method = "lm", col = "#619CFF")


# In progress decay curve graphs
df %>%
  filter(SLC == "Ph2.Pine") %>%
  add_predictions(noa_model) %>%
  ggplot(aes(x = days_to_collection, y = PercMassRemaining, group = treatment)) +
  geom_point() +
  geom_line(aes(x = days_to_collection, y = pred)) +
  ggtitle("Decay Curve: Morella Treatment")

df %>%
  filter(treatment == "Morella") %>%
  add_predictions(noa_model) %>%
  ggplot(aes(x = years_to_collection, y = PercMassRemaining)) +
  geom_point() +
  geom_smooth(aes(y = pred)) +
  ylim(0.7, 1)



# trying to plot a singular noa_model with ggplot and geom_smooth... not working yet
ggplot(
  data = df %>% filter(treatment == "Morella"),
  aes(
    x = years_to_collection,
    y = PercMassRemaining
  )
) +
  geom_point() +
  geom_smooth(
    method = "nls",
    formula = y ~ 1 * exp(-a * x),
    method.args = list(
      start = list(a = 0.02)
    )
  )

noa_model


df %>%
  ggplot(aes(x = years_to_collection)) +
  geom_point(col = "dark gray", aes(y = pred)) +
  geom_line(aes(group = treatment, col = treatment, y = pred)) +
  geom_smooth(aes(y = pred),
    method = "nls",
    formula = "PercMassRemaining ~ 1 * exp(-a * years_to_collection)",
    method.args = list(
      start = list(
        a = 0.1,
        PercMassRemaining = 1,
        years_to_collection = 0
      ),
      alpha = 0.3
    )
  )

+
  ggtitle("Decay Curves Predicted by Modeled Decay Coefficients")

add_predictions(
  df %>%
    filter(treatment == "Morella"),
  model = morella_model
) %>%
  ggplot(aes(x = years_to_collection)) +
  geom_line(aes(group = treatment, col = treatment, y = pred))


ggplot(SLC_alphas_tbl, aes(x = reorder(SLC, -a), y = a)) +
  geom_bar(stat = "identity", width = 0.5, aes(fill = litter)) +
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error), width = 0.2) +
  theme(axis.text.x = element_text(angle = 90))


# litter type regressions
df_phrag_regression <- df %>%
  filter(class == "Phragmites")

phrag_regression_sal <- df_phrag_regression %>%
  lm(formula = mean_salinity ~ a)
summary(phrag_regression_sal)

phrag_regression_mois <- df_phrag_regression %>%
  lm(formula = a ~ mean_moisture)
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
  filter(class == "Pine")

pine_regression_sal <- df_pine_regression %>%
  lm(formula = mean_salinity ~ a)
summary(pine_regression_sal)

pine_regression_mois <- df_pine_regression %>%
  lm(formula = mean_moisture ~ a)
summary(pine_regression_mois)

ggplot(data = df_pine_regression, aes(x = mean_salinity, y = a)) +
  geom_point(col = "#619CFF") +
  geom_smooth(method = "lm", col = "#619CFF") +
  ggtitle("Impact of Soil Salinity on Decay (Pine Litter)") +
  xlab("Mean Soil Salinity") +
  ylab("Decay Rate") +
  annotate(
    geom = "text",
    label = "R^2 = 0.2752",
    fontface = 2,
    x = 1,
    y = 0.16
  )

ggplot(data = df %>% filter(!is.na(class)), aes(x = mean_moisture, y = a)) +
  geom_point(aes(col = class)) +
  geom_smooth(
    data = df_phrag_regression,
    method = "lm",
    col = "#00BA38",
    fill = "#00BA38",
    alpha = 0.3
  ) +
  geom_smooth(
    data = df_mor_regression,
    method = "lm",
    col = "#F8766D",
    fill = "#F8766D",
    alpha = 0.3
  ) +
  geom_smooth(
    data = df_pine_regression,
    method = "lm",
    col = "#619CFF",
    fill = "#619CFF",
    alpha = 0.3
  ) +
  ggtitle("Impact of Soil Moisture on Decay") +
  xlab("Mean Soil Moisture") +
  ylab("Decay Rate") +
  scale_color_discrete(name = "Litter") +
  annotate(
    geom = "text",
    label = "Phragmites R^2 = 0.2557",
    fontface = 2,
    x = 0.72,
    y = 0.14,
    color = "#00BA38"
  ) +
  annotate(
    geom = "text",
    label = "Pine R^2 = 0.2694",
    fontface = 2,
    x = 0.74,
    y = 0.127,
    color = "#619CFF"
  ) +
  annotate(
    geom = "text",
    label = "Morella R^2 = 0.1389",
    fontface = 2,
    x = 0.731,
    y = 0.115,
    color = "#F8766D"
  )

# multivariate regressions

multivariate_pine <- lm(df_pine_regression, formula = a ~ mean_salinity * mean_moisture)
summary(multivariate_pine)

multivariate_phrag <- lm(df_phrag_regression, formula = a ~ mean_salinity * mean_moisture)
summary(multivariate_phrag)

multivariate_mor <- lm(df_mor_regression, formula = a ~ mean_salinity * mean_moisture)
summary(multivariate_mor)

# I'm crazy I'm crazy I want to try Treatment litter combos now
TLC_alphas <- df %>%
  nest(-TLC) %>%
  mutate(
    fit = map(data, ~ nls(PercMassRemaining ~ 1 * exp(-a * years_to_collection), data = ., start = list(a = 0.1))),
    tidied = map(fit, tidy),
    Augmented = map(fit, augment),
  )

TLC_alphas_tbl <-
  TLC_alphas %>%
  unnest(tidied) %>%
  select(TLC, term, estimate, std.error) %>%
  spread(term, estimate)


TLC_alphas_tbl

df <- df %>%
  merge(TLC_alphas_tbl, by = "TLC")
