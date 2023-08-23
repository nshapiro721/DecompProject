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
)


# Iterating models of alpha for all SLC's
SLC_alphas <- df %>%
  nest(-SLC) %>%
  mutate(
    fit = map(
      data,
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

site_regression_sal <- df_site_regressions %>%
  lm(formula = mean_salinity ~ mean_alpha)
summary(site_regression_sal)

site_regression_mois <- df_site_regressions %>%
  lm(formula = mean_moisture ~ mean_alpha)
summary(site_regression_mois)


multivariate_site <- lm(df_site_regressions, formula = mean_alpha ~ mean_salinity * mean_moisture)
summary(multivariate_site)

# Graphing the impacts/lack thereof
