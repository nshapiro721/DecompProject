source("exec/read_data.R")

# here's a NLS example without self-starting.
cool_model <- nls(
  post_litter_mass ~ init_mass_litter * exp(-a * years_to_collection),
  data = df %>% filter(treatment == "Morella"),
  start = list(a = 0.02)
)
cool_model

