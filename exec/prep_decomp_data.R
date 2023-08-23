# read in multiple raw files to be combined into single dataframe called "df"
df <- read.csv("data/raw/decomp_data.csv")
sitedf <- read.csv("data/raw/site_contents.csv")[, c(1, 4)]
initials <- read.csv("data/raw/initials.csv")
df <- merge(df, sitedf, by = "tag")

# replace incorrect days_to_collection values
# see "data/days_to_collection_fix.csv"
dtc_lookup <- read.csv("data/raw/days_to_collection_fix.csv")
df$days_to_collection = sapply(
    X  = df$days_to_collection,
    FUN = function(dtc) {
        idx <- which(dtc == dtc_lookup$originally_listed_days)
        if(length(idx) == 0) return(dtc)
        else return(dtc_lookup$corrected_days[idx])
    }
)

# add relevant columns -- SLC is shorthand for Site-Litter Combination
df$SLC <- interaction(as.factor(df$site), as.factor(df$class))
df$PercMassRemaining <- 1 - (df$init_total_mass - df$post_total_mass) / df$init_mass_litter
df$post_litter_mass <- df$init_mass_litter - df$mass_loss

# combine with initial mass @ 0 days to collection
df <- dplyr::bind_rows(df, initials)

# this has to occur after initials get added
df$years_to_collection <- df$days_to_collection / 365