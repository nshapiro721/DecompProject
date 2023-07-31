#Lookin at soil data

library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(purrr)
library(modelr)
source("exec/util.R")

setwd("/Users/noashapiro-tamir/Documents/dev/DecompProject")

#Importing data
soils_bvl <- read.csv("data/All_plots_soils.csv")
soils_decomp <- read.csv("data/Decomp_soils.csv")

#fixing little things
soils_decomp[21, 2] = "Pi1"
soils_decomp <- soils_decomp %>% 
  mutate(treatment = if_else(site == "Mo1"| site == "Mo2" | site == "Mo3", "Morella", "Other")) %>%
  mutate(treatment = if_else(site == "Pi1"| site == "Pi2" | site == "Pi3", "Pine", "Other")) %>%
  mutate(treatment = if_else(site == "Ph1"| site == "Ph2" | site == "Ph3", "Phragmites", "Other"))
  
  

#initial graphs
ggplot(data = soils_decomp, aes(x=site, y=Salinity_ppt)) + geom_point()
ggplot(data = soils_decomp, aes(x=site, y=percentage_moisture)) + geom_point()
ggplot(data = soils_decomp, aes(x=Salinity_ppt, y=percentage_moisture, col = site)) + geom_point()
ggplot(data = soils_decomp, aes(y=percentage_moisture, group=site)) + geom_boxplot()

