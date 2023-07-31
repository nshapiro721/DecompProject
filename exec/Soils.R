# Lookin at soil data

library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(purrr)
library(modelr)
source("exec/util.R")

setwd("/Users/noashapiro-tamir/Documents/dev/DecompProject")

# Importing data
soils_bvl <- read.csv("data/All_plots_soils.csv")
soils_decomp <- read.csv("data/Decomp_soils.csv")

# fixing little things
soils_decomp[21, 2] <- "Pi1"
soils_bvl[84, 11] <- 20200
soils_bvl[84, 12] <- 10.06
soils_bvl <- soils_bvl %>%
  mutate(EC = as.numeric(EC_uS.cm),
         salinity = as.numeric(Salinity_ppt),
         QC_EC = as.numeric(QC.rdg.EC))


soils_decomp <- soils_decomp %>%
  mutate(treatment = if_else(site == "Mo1" | site == "Mo2" | site == "Mo3", "Morella", "Other")) %>%
  mutate(treatment = if_else(site == "Pi1" | site == "Pi2" | site == "Pi3", "Pine", "Other")) %>%
  mutate(treatment = if_else(site == "Ph1" | site == "Ph2" | site == "Ph3", "Phragmites", "Other"))



# initial graphs of decomp stuff
ggplot(data = soils_decomp, aes(x = site, y = Salinity_ppt)) +
  geom_point()
ggplot(data = soils_decomp, aes(x = site, y = percentage_moisture)) +
  geom_point()
ggplot(data = soils_decomp, aes(x = Salinity_ppt, y = percentage_moisture, col = site)) +
  geom_point()
ggplot(data = soils_decomp, aes(y = percentage_moisture, group = site)) +
  geom_boxplot()

#looking at plot data

ggplot(data = soils_bvl, aes(y = EC, x = plot, col = zone)) +
  geom_point()

replicated <- soils_bvl %>%
  filter(!is.na(QC.rdg.EC)) %>%
  filter(QC.rdg.EC != 10100)

fit_EC <- lm(formula = EC ~ QC_EC, data = replicated)
summary(fit_EC)

ggplot(data = replicated, aes(x = EC, y = QC_EC)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)
        
fit_sal <- lm(formula = salinity ~ QC.rdg.salinity, data = replicated)
summary(fit_sal)

ggplot(data = replicated, aes(x = salinity, y = QC.rdg.salinity)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)

