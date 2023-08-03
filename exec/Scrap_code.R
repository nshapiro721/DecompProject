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


#joining...
df %>%
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

SLC_plot <- ggplot(data = df, aes(x = SLC, y = SLC_a)) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_point(aes(colour = treatment))
SLC_plot


