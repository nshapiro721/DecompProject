library(dplyr)
library(ggplot2)

#Importing data
df <- read.csv("data/decomp_data.csv")
sitedf <- read.csv("data/site_contents.csv")[,c(1,4)]
df <- merge(df, sitedf, by = "tag")
df$SLC <- interaction(as.factor(df$site), as.factor(df$class))
df$PercMassRemaining <- 1 - (df$init_total_mass - df$post_total_mass)/df$init_mass_litter

#setting up for fit curves: copied from internet

nls(PercMassRemaining ~ yf + (y0 - yf) * exp(-alpha * Day), 
    data = Pi1_Pine,
    start = list(y0 = 1, yf = 0.96, alpha = 0.008))

fit <- nls(PercMassRemaining ~ SSasymp(Day, yf, y0, log_alpha), data = Pi1_Pine)


ggplot(data = df %>% filter(class == "Pine")) + 
  aes(x = days_to_collection, y = PercMassRemaining, color = SLC) +
  geom_line() +
  facet_wrap(vars(site), ncol = 3)

df2 <- df
df2$days_to_collection[which(df$days_to_collection == 84)] = 182
df2$days_to_collection[which(df$days_to_collection == 182)] = 84



ggplot(data = df) + 
  aes(x = days_to_collection) +
  geom_line(aes(y = init_total_mass, color = SLC))


plot(Pi1_Pine$Day, Pi1_Pine$PercMassRemaining)
curve(x^-0.006, from=0, to=365, add=TRUE, col = "violet")

plot(Pi1_Pine$Day, Pi1_Pine$Dry_Mass)
curve(x^-0.45, from=0, to=365, add=TRUE, col = "violet")

plot(Pi1_Phrag$Day, Pi1_Phrag$PercMassRemaining)
curve(x^-0.006, from=0, to=365, add=TRUE, col = "violet")

plot(Pi1_Morella$Day, Pi1_Mor$PercMassRemaining)
curve(x^-0.006, from=0, to=365, add=TRUE, col = "violet")

plot(Mo1_Pine$Day, Mo1_Pine$PercMassRemaining)
curve(x^-0.009, from=0, to=365, add=TRUE, col = "violet")

plot(Ph3_Morella$Day, Ph3_Morella$PercMassRemaining, ylim = c(0.8,1))
curve(x^-0.012, from=0, to=365, add=TRUE, col = "violet", lwd = 8)




# Fit the data
fitted <- Data %>% 
  nest(-treatment) %>%
  mutate(
    fit = map(data, ~nls(y ~ SSasymp(t, yf, y0, log_alpha), data = .)),
    tidied = map(fit, tidy),
    augmented = map(fit, augment),
  )  

# Produce a table of fit parameters: y0, yf, alpha
fitted %>% 
  unnest(tidied) %>% 
  select(sensor, term, estimate) %>% 
  spread(term, estimate) %>% 
  mutate(alpha = exp(log_alpha))



