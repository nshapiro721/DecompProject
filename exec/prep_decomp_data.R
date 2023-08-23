df <- read.csv("data/decomp_data.csv")
sitedf <- read.csv("data/site_contents.csv")[, c(1, 4)]
initials <- read.csv("data/initials.csv")

df <- merge(df, sitedf, by = "tag")
df$SLC <- interaction(as.factor(df$site), as.factor(df$class))
df$PercMassRemaining <- 1 - (df$init_total_mass - df$post_total_mass) / df$init_mass_litter
df$post_litter_mass <- df$init_mass_litter - df$mass_loss
df <- bind_rows(df, initials)
df[117, 3] <- "Phragmites"

dtc_lookup <- read.csv("data/days_to_collection_fix.csv")
df$days_to_collection = lapply(
    X  = df$days_to_collection,
    FUN = function(dtc) {
        dtc_lookup$corrected_days[which(dtc == dtc_lookup$originally_listed_days)]
    }
)

df$years_to_collection <- df$days_to_collection / 365

# SOIL DATA
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
