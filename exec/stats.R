# anova & tukey tests of site/litter interactions
SLC_anova <- aov(data = SLC_alphas_tbl, a ~ treatment * litter)
summary(SLC_anova)
TukeyHSD(SLC_anova, "treatment")


#  regressions to find statistical significance of the soil salinity and moisture impacts on  site decay rate

df_site_regressions <- df %>%
  group_by(site) %>%
  summarize(
    mean_alpha = mean(a),
    mean_salinity = mean(mean_salinity),
    mean_moisture = mean(mean_moisture)
  ) %>%
  mutate(treatment = substr(site, 1, 2))