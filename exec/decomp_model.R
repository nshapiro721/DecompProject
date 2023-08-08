library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(purrr)
library(modelr)
library(stringr)
source("exec/util.R")

# Importing data
df <- read.csv("data/decomp_data.csv")
sitedf <- read.csv("data/site_contents.csv")[, c(1, 4)]
initials <- read.csv("data/initials.csv")
#initials$treatment <- substr(initials$SLC, 1, 2) 
#initials$litter <- substr(initials$SLC, 5, 7)
#initials$TLC <- interaction(as.factor(initials$treatment), as.factor(initials$litter))
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

df$years_to_collection <- df$days_to_collection/365

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

# here's a NLS example without self-starting.
cool_model <- nls(
  post_litter_mass ~ init_mass_litter * exp(-a * years_to_collection),
  data = df %>% filter(treatment == "Morella"),
  start = list(a = 0.02)
)
cool_model

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

# Noa makes the same model but with percents, and makes one for each of the three treatments, since we'll need them to graph the curves later. Has a lower residual sum of squares so kept it.

morella_model <- nls(
  PercMassRemaining ~ 1 * exp(-a * years_to_collection),
  data = df %>% filter(treatment == "Morella"),
  start = list(a = 0.02)
                )

morella_model

phrag_model <- nls(
  PercMassRemaining ~ 1 * exp(-a * years_to_collection),
  data = df %>% filter(treatment == "Phragmites"),
  start = list(a = 0.02)
)

pine_model <- nls(
  PercMassRemaining ~ 1 * exp(-a * years_to_collection),
  data = df %>% filter(treatment == "Pine"),
  start = list(a = 0.02)
)


# Testing Noa model - trying to do here what Riley did with Cool model. Not sure if it actually shows us new info or not.
t_to_eyeball_fit <- 1:365
perc_mass_to_eyeball_fit_noa <- get_mass_pct_remaining_at_t(
  a = coef(noa_model)[["a"]],
  t = t_to_eyeball_fit
)
plot(
  x = df[which(df$treatment == "Morella"), "years_to_collection"],
  y = df[which(df$treatment == "Morella"), "PercMassRemaining"]
)
lines(
  x = t_to_eyeball_fit,
  y = perc_mass_to_eyeball_fit_noa,
  t = "p",
  col = "blue"
)

# Iterating noa_model (now morella_model) for all treatments
treatment_alphas <- df %>%
  nest(-treatment) %>%
  mutate(
    fit = map(data, ~ nls(PercMassRemaining ~ 1 * exp(-a * years_to_collection), data = ., start = list(a = 0.1))),
    tidied = map(fit, tidy),
    Augmented = map(fit, augment),
  )

treatment_alphas_tbl <- treatment_alphas %>%
  unnest(tidied) %>%
  select(treatment, term, estimate, std.error) %>%
  spread(term, estimate)

treatment_alphas_tbl

#graphing treatment alphas with standard error
   #Bar version
ggplot(treatment_alphas_tbl, aes(x = treatment, y = a)) +
  geom_bar(stat = "identity", width = 0.5, fill = "light blue") +
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error), width = 0.2)

   #Point version (preferred)
ggplot(treatment_alphas_tbl, aes(x = treatment, y = a)) +
  geom_point(aes(col = treatment), size = 2) +
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error, col = treatment), width = 0.1) +
  ggtitle("Decay Coefficient (k) for each Decay Environment \n (with standard error)") +
  xlab("Decay Environment") +
  ylab("k") +
  theme(legend.position = "none")

# Iterating noa_model (now morella_model) for all Site/Treatment combos
SLC_alphas <- df %>%
  nest(-SLC) %>%
  mutate(
    fit = map(data, ~ nls(PercMassRemaining ~ 1 * exp(-a * years_to_collection), data = ., start = list(a = 0.01))),
    tidied = map(fit, tidy),
    Augmented = map(fit, augment),
  )

SLC_alphas_tbl <-
  SLC_alphas %>%
  unnest(tidied) %>%
  select(SLC, term, estimate, std.error) %>%
  spread(term, estimate)


SLC_alphas_tbl

df <- df %>%
  merge(SLC_alphas_tbl, by = "SLC")

# graphs of k values
    #before joining

SLC_alphas_tbl <- as.data.frame(SLC_alphas_tbl) 
SLC_alphas_tbl$site <- substr(SLC_alphas_tbl$SLC, 1, 3) 
SLC_alphas_tbl$treatment <- substr(SLC_alphas_tbl$SLC, 1, 2)
SLC_alphas_tbl$litter = rep(c("Morella", "Phragmites", "Pinus"), 9)
SLC_alphas_tbl$litter = as.factor(SLC_alphas_tbl$litter)

ggplot(SLC_alphas_tbl, aes(x = reorder(SLC, -a), y = a)) +
  geom_point(aes(col = treatment), size = 2) +
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error
                    , col = treatment
                    ), 
                width = 0.08) + 
  scale_color_discrete(name = "Decay\nEnvironment") +
  theme(axis.text.x = element_text(angle = 90)
      #  , legend.title = element_text("Decay Environment")
 ) +
  ggtitle("Decay Coefficient (k) for each Site/Litter Combination (SLC)") +
  xlab("SLC") + 
  ylab("k")


ggplot(SLC_alphas_tbl, aes(x=litter, y=a, group = treatment))+
  geom_point(aes(color=treatment))+
  geom_line(aes(color=treatment))+
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error), width = 0.2)
# ^each site as a different point

SLC_alphas_sum = SLC_alphas_tbl%>% group_by(treatment, litter) %>% 
  summarise(mean_a = mean(a),
            std.error = sd(a)/sqrt(length(a)))

#interaction graph
ggplot(SLC_alphas_sum, aes(x=treatment, y=mean_a, group = litter, color=litter))+
  geom_point(position=position_dodge(width=0.2), size = 1.5)+
  geom_errorbar(aes(ymin = mean_a - std.error, ymax = mean_a + std.error), width = 0.35, position=position_dodge(width=0.2), linetype = 5) + 
  geom_line(position = position_dodge(width = 0.2), linetype = 1) +
  ggtitle("Impact of Decay Environment/Litter Interaction\non Decay Rate (k) ") +
  xlab("Decay Environment") +
  ylab("k") +
  scale_color_discrete(name = "Litter") +
  theme(
    panel.grid.major = element_blank()
  #      , 
    #    panel.grid.minor = element_blank()
        )

#bar version, which I like less
ggplot(SLC_alphas_sum, aes(x=treatment, y=mean_a, group = litter, fill=litter))+
  geom_bar(stat = "identity", position = position_dodge(width=0.6), width = 0.5)+
  geom_errorbar(aes(ymin = mean_a - std.error, ymax = mean_a + std.error), width = 0.2
               , position=position_dodge(width=0.6)
                )

SLC_anova = aov(data=SLC_alphas_tbl, a ~ treatment*litter)
summary(SLC_anova)
TukeyHSD(SLC_anova, "treatment")

# plotting the curves themselves

  #first, some tests...
four_years <- as.data.frame(seq(1, 4, 0.02))

%>%
  rename(four_years_continuous = seq)

ten_years <- as.data.frame(seq(1, 10, 0.02))
noa_model_tester <- nls(
  PercMassRemaining ~ 1 * exp(-a * years_to_collection),
  data = df %>% filter(treatment == "Morella"),
  start = list(a = 0.02)
)

noa_model_tester

t_to_eyeball_fit <- 1:1000

perc_mass_to_eyeball_fit_four <- four_years %>% 
  rename("year" = "seq(1, 4, 0.02)") %>%
  mutate (pmr = unlist(get_mass_pct_remaining_at_t(
    alpha = coef(noa_model_tester)[["a"]],
    t = four_years)))

perc_mass_to_eyeball_fit_ten <- ten_years %>% 
  rename("year" = "seq(1, 10, 0.02)") %>%
  mutate (pmr = unlist(get_mass_pct_remaining_at_t(
    alpha = coef(noa_model_tester)[["a"]],
    t = ten_years)))


perc_mass_to_eyeball_fit

ggplot(data = perc_mass_to_eyeball_fit_ten, aes(x = year, y = pmr)) +
  geom_point(data = df %>%
               filter(treatment == "Morella") %>%
               add_predictions(model = morella_model),
             aes(x = years_to_collection, y = PercMassRemaining)) + 
  geom_point(col = "red")

  #now, the real thing

df <- df %>% mutate(ID = row_number())

morella_preds <- df %>% filter(treatment == "Morella") %>%
  add_predictions(morella_model) %>%
  select(pred, ID)

phrag_preds <- df %>% filter(treatment == "Phragmites") %>%
  add_predictions(phrag_model) %>%
  select(pred, ID)

pine_preds <- df %>% filter(treatment == "Pine") %>%
  add_predictions(pine_model) %>%
  select(pred, ID)

preds <- rbind(morella_preds, phrag_preds, pine_preds)

df<- merge(preds, df, by = "ID")

ggplot(data = df,
       aes(x = years_to_collection, y = PercMassRemaining)) +
  geom_point(col = "dark gray") +
  geom_smooth(aes(y = pred, 
                  group = treatment, 
                  col = treatment, 
                  fill = treatment), 
              alpha = 0.3) +
  ggtitle("Modeled Decay Rate Curves for each Decay Environment") +
  labs(subtitle = "    with 95% confidence interval") +
  xlab("Years to Collection") +
  xlim(c(0,1)) +
  ylab("Percent Mass Remaining") +
  scale_color_discrete(name = "Decay\nEnvironment") +
  scale_fill_discrete(name = "Decay\nEnvironment")

#Bringing in soil data
soils_decomp <- read.csv("data/Decomp_soils.csv")
soils_decomp[21, 2] <- "Pi1"
soils_decomp_clean <- 
  soils_decomp %>% 
    group_by (site) %>%
    summarise(mean_moisture = mean(percentage_moisture), 
              mean_EC = mean(EC_uS.cm), 
              mean_salinity = mean(Salinity_ppt))
df <- df %>%
  merge(soils_decomp_clean, by = "site")

#making regressions of soils + decomp data

df_phrag_regression <- df %>% 
  filter(class == "Phragmites")

    phrag_regression_sal <- df_phrag_regression %>%
      lm(formula = mean_salinity ~ a)
    summary(phrag_regression_sal)
  
    phrag_regression_mois <- df_phrag_regression %>%
      lm(formula = a ~ mean_moisture)
    summary(phrag_regression_mois)

df_mor_regression <- df %>%
  filter(class == "Morella")

    mor_regression_sal <- df_mor_regression %>%
     lm(formula = mean_salinity ~ a)
    summary(mor_regression_sal)
    
    mor_regression_mois <- df_mor_regression %>%
      lm(formula = mean_moisture ~ a)
    summary(mor_regression_mois)

df_pine_regression <- df %>%
  filter(class == "Pine")

    pine_regression_sal <- df_pine_regression %>%
     lm(formula = mean_salinity ~ a)
    summary(pine_regression_sal)

    pine_regression_mois <- df_pine_regression %>%
      lm(formula = mean_moisture ~ a)
    summary(pine_regression_mois)

ggplot(data = df_pine_regression, aes(x = mean_salinity, y = a)) + 
  geom_point(col = "#619CFF") +
  geom_smooth(method = "lm", col = "#619CFF") +
  ggtitle("Impact of Soil Salinity on Decay (Pine Litter)") +
  xlab("Mean Soil Salinity") + 
  ylab("Decay Rate") +
  annotate(geom = "text",
           label = "R^2 = 0.2752",
           fontface = 2,
           x = 1,
           y = 0.16)

ggplot(data= df %>% filter(!is.na(class)), aes(x = mean_moisture, y = a)) + 
   geom_point(aes(col = class)) +
   geom_smooth(data = df_phrag_regression, 
               method = "lm", 
               col = "#00BA38", 
               fill = "#00BA38", 
               alpha = 0.3) +
   geom_smooth(data = df_mor_regression, 
               method = "lm", 
               col = "#F8766D", 
               fill = "#F8766D", 
               alpha = 0.3) +
   geom_smooth(data = df_pine_regression, 
               method = "lm", 
               col = "#619CFF", 
               fill = "#619CFF", 
               alpha = 0.3) +
   ggtitle("Impact of Soil Moisture on Decay") +
   xlab("Mean Soil Moisture") +
   ylab("Decay Rate") +
   scale_color_discrete(name = "Litter") +  
   annotate(geom = "text",
          label = "Phragmites R^2 = 0.2557",
          fontface = 2,
          x = 0.72,
          y = 0.14,
          color = "#00BA38") +
  annotate(geom = "text",
           label = "Pine R^2 = 0.2694",
           fontface = 2,
           x = 0.74,
           y = 0.127,
           color = "#619CFF") +
  annotate(geom = "text",
           label = "Morella R^2 = 0.1389",
           fontface = 2,
           x = 0.731,
           y = 0.115,
           color = "#F8766D")
  
#multivariate regressions

multivariate_pine <- lm(df_pine_regression, formula = a ~ mean_salinity * mean_moisture)
summary(multivariate_pine)

multivariate_phrag <- lm(df_phrag_regression, formula = a ~ mean_salinity * mean_moisture)
summary(multivariate_phrag)

multivariate_mor <- lm(df_mor_regression, formula = a ~ mean_salinity * mean_moisture)
summary(multivariate_mor)

#I'm crazy I'm crazy I want to try Treatment litter combos now
TLC_alphas <- df %>%
  nest(-TLC) %>%
  mutate(
    fit = map(data, ~ nls(PercMassRemaining ~ 1 * exp(-a * years_to_collection), data = ., start = list(a = 0.1))),
    tidied = map(fit, tidy),
    Augmented = map(fit, augment),
  )

TLC_alphas_tbl <-
  TLC_alphas %>%
  unnest(tidied) %>%
  select(TLC, term, estimate, std.error) %>%
  spread(term, estimate)


TLC_alphas_tbl

df <- df %>%
  merge(TLC_alphas_tbl, by = "TLC")

# graphs of k values
#before joining

SLC_alphas_tbl <- as.data.frame(SLC_alphas_tbl) 
SLC_alphas_tbl$site <- substr(SLC_alphas_tbl$SLC, 1, 3) 
SLC_alphas_tbl$treatment <- substr(SLC_alphas_tbl$SLC, 1, 2)
SLC_alphas_tbl$litter = rep(c("Morella", "Phragmites", "Pinus"), 9)
SLC_alphas_tbl$litter = as.factor(SLC_alphas_tbl$litter)

ggplot(SLC_alphas_tbl, aes(x = reorder(SLC, -a), y = a)) +
  geom_point(aes(col = litter), size = 2) +
  geom_errorbar(aes(ymin = a - std.error, ymax = a + std.error
                    #       , col = treatment
  ), 
  width = 0.08) + 
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Decay Coefficient (k) for each Litter at each Site")

