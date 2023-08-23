# Figures not used in final presentation and/or analysis. Some may be incomplete.

source("exec/read_data.R")

# visualizing the weird bump at the third collection date
ggplot(df, aes(x = days_to_collection, y = PercMassRemaining)) +
  geom_point() +
  ggtitle("Raw Data") +
  xlab("Days to Collection") +
  ylab("Percent Mass Remaining")


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

# Bar version of treatment alphas
ggplot(treatment_alphas_tbl, aes(x = treatment, y = a)) +
  geom_bar(stat = "identity", width = 0.5, fill = "light blue") +
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error), width = 0.2)


# graphing treatment alphas with standard error from treatment model
ggplot(treatment_alphas_tbl, aes(x = treatment, y = a)) +
  geom_point(aes(col = treatment), size = 2) +
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error, col = treatment), width = 0.1) +
  ggtitle("Decay Coefficient (k) for each Decay Environment \n (with standard error)") +
  xlab("Decay Environment") +
  ylab("k") +
  theme(legend.position = "none") +
  ylim(c(0.19, 0.29))

# each site as its own point
ggplot(SLC_alphas_tbl, aes(x = treatment, y = a, group = treatment)) +
  geom_point(aes(color = litter)) +
  geom_line(aes(color = treatment))

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

# bar version of interaction graph, which I like less
ggplot(SLC_alphas_sum, aes(x = treatment, y = mean_a, group = litter, fill = litter)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  geom_errorbar(aes(ymin = mean_a - std.error, ymax = mean_a + std.error),
    width = 0.2,
    position = position_dodge(width = 0.6)
  )

# treatment by moisture barplot  (with error bars)
ggplot(soils_decomp_clean_treatment, aes(y = mean_moisture)) +
  geom_bar(stat = "identity", aes(x = treatment, fill = treatment), width = 0.4) +
  geom_errorbar(aes(x = treatment, ymin = mean_moisture - std.error_mois, ymax = mean_moisture + std.error_mois), width = 0.15)

# treatment by salinity barplot  (with error bars)
ggplot(soils_decomp_clean_treatment, aes(y = mean_salinity)) +
  geom_bar(stat = "identity", aes(x = treatment, fill = treatment), width = 0.4) +
  geom_errorbar(aes(x = treatment, ymin = mean_salinity - std.error_sal, ymax = mean_salinity + std.error_sal), width = 0.15)

# showing how the low moisture outlier skews the whole thing terribly
soils_decomp_clean_site %>%
  filter(mean_moisture > 0.6) %>%
  ggplot(aes(x = mean_moisture, y = mean_salinity)) +
  geom_point(aes(col = treatment)) +
  geom_smooth(method = "lm")
