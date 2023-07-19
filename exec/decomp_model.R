library(dplyr)
library(ggplot2)
source("exec/util.R")

#Importing data
df <- read.csv("data/decomp_data.csv")
sitedf <- read.csv("data/site_contents.csv")[,c(1,4)]
df <- merge(df, sitedf, by = "tag")
df$SLC <- interaction(as.factor(df$site), as.factor(df$class))
df$PercMassRemaining <- 1 - (df$init_total_mass - df$post_total_mass)/df$init_mass_litter
df$post_litter_mass = df$init_mass_litter - df$mass_loss


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

# here's a NLS example without self-starting.
cool_model <- nls(
  post_litter_mass ~ init_mass_litter * exp(-a * days_to_collection), 
  data = df %>% filter(treatment == "Morella"),
  start = list(a = 0.008)
)

# here's a sanity check for the estimate of alpha that it spits out
# you'll need to do the actual stats on it but here's a gander
# get_mass_pct_remaining_at_t is also a function from "exec/util.R"
t_to_eyeball_fit <- 1:365
perc_mass_to_eyeball_fit <- get_mass_pct_remaining_at_t(
  a = coef(cool_model)[['a']], 
  t = t_to_eyeball_fit
)
plot(
  x = df[which(df$treatment == "Morella"),"days_to_collection"], 
  y = df[which(df$treatment == "Morella"),"PercMassRemaining"]
)
lines(
  x = t_to_eyeball_fit,
  y = perc_mass_to_eyeball_fit,
  t = "p",
  col = "blue"
)


