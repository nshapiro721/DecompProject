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
    panel.grid.major = element_blank()
  )


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

perc_mass_to_eyeball_fit_ten <- ten_years %>%
  rename("years_to_collection" = "seq(1, 10, 0.02)") %>%
  mutate(
    Morella =
      unlist(get_mass_pct_remaining_at_t(
        alpha = coef(models$Morella)[["a"]],
        t = ten_years
      )),
    Phragmites =
      unlist(get_mass_pct_remaining_at_t(
        alpha = coef(models$Phragmites)[["a"]],
        t = ten_years
      )),
    Pine =
      unlist(get_mass_pct_remaining_at_t(
        alpha = coef(models$Pine)[["a"]],
        t = ten_years
      ))
  ) %>%
  pivot_longer(
    cols = c("Morella", "Phragmites", "Pine"),
    names_to = "treatment",
    values_to = "pmr"
  )

# Graph showing decay curves for each environment over 10 years
ggplot(data = perc_mass_to_eyeball_fit_ten, aes(x = years_to_collection)) +
  geom_smooth(aes(
    y = pmr,
    group = treatment,
    col = treatment
  )) +
  geom_smooth(
    data = df,
    aes(y = pred, col = treatment), se = FALSE
  ) +
  geom_point(
    data = df,
    aes(x = years_to_collection, y = PercMassRemaining)
  ) +
  ggtitle("Decay Curves of each Decay Environment Over 10 Years") +
  ylab("Percent Mass Remaining") +
  xlab("Years") +
  scale_color_discrete(
    labels = c("Morella", "Phragmites", "Pine"),
    name = "Decay\nEnvironment"
  ) +
  theme(legend.position = c(0.85, 0.7))


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
