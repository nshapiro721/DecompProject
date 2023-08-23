soils_decomp <- read.csv("data/raw/decomp_soils_no_metadata.csv")
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
