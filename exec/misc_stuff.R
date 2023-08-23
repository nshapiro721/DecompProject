# tests of the model for each treatment
deviance(models$Morella)
deviance(models$Phragmites)
deviance(models$Pine)


# Iterating treatment model for all treatments (this does probably the same thing as Riley
# accomplished above, but is what I did originally, as instructed by the Douglas Watson article on
# non-linear modeling of decay: https://douglas-watson.github.io/post/2018-09_exponential_curve_fitting/ )

treatment_alphas <- df %>%
  nest(data = -treatment) %>%
  mutate(
    fit = map(
      data,
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