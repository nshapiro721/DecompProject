df <- read.csv("data/decomp_data.csv")
sitedf <- read.csv("data/site_contents.csv")[, c(1, 4)]
initials <- read.csv("data/initials.csv")
df <- merge(df, sitedf, by = "tag")
df$SLC <- interaction(as.factor(df$site), as.factor(df$class))
df$TLC <- interaction(as.factor(df$treatment), as.factor(df$class))
df$PercMassRemaining <- 1 - (df$init_total_mass - df$post_total_mass) / df$init_mass_litter
df$post_litter_mass <- df$init_mass_litter - df$mass_loss
df <- bind_rows(df, initials)
df[117, 3] <- "Phragmites"
# Changing the days_to_collection values to reflect accurate collection dates
df <- df %>%
  mutate(days_to_collection = if_else(days_to_collection == 28, 22, days_to_collection)) %>%
  mutate(days_to_collection = if_else(days_to_collection == 84, 76, days_to_collection)) %>%
  mutate(days_to_collection = if_else(days_to_collection == 182, 163, days_to_collection)) %>%
  mutate(days_to_collection = if_else(days_to_collection == 365, 345, days_to_collection))

df$years_to_collection <- df$days_to_collection / 365
