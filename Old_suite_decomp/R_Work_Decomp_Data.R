# Data Work for Decomposition Project
# Noa Shapiro-Tamir, Gedan Lab REU 2023
# 06.30.2023

# setup
library(stats)
library(dplyr)
library(broom)
library(ggplot2)

setwd("/Users/noashapiro-tamir/Desktop/R_Files/DecompProject/")

# Importing data
Data <- read.csv("Decomp_Data_Edited_Noa_Raw_1.csv")
Data_bysite <- read.csv("Decomp_Data_Edited_Noa_Site_Contents_3.csv")

# Cleaning data to prep for merging
colnames(Data)[1] <- "Tag"
colnames(Data)[5] <- "Initial_bag_mass"
colnames(Data)[6] <- "Initial_litter_mass"
colnames(Data)[7] <- "Initial_bag_litter_mass"
colnames(Data)[8] <- "Dry_Mass"
colnames(Data)[10] <- "Day"
colnames(Data_bysite)[4] <- "Tag"
Data_bysite <- Data_bysite[, c(1, 4)]
Data$PercMassRemaining <- Data$Dry_Mass / Data$`Initial_bag_litter_mass`
Data$PercMassInitial <- Data$Initial_bag_litter_mass / Data$Initial_bag_litter_mass
Data$Years <- Data$Day / 365

# merging site name to Data file so that we can stratify curves later
Data <- merge(Data, Data_bysite, by = "Tag")

# Making a column for unique values of site and litter type together
Data$SiteLitterCombo <- interaction(as.factor(Data$Site), as.factor(Data$Litter.type))

# Excluding the 6-month (182 day) data points because they skew the data very confusingly.
Data2 <- Data[Data$Day != 182, ]


# creating boxplots of all data points to check for outliers.
boxplot(Data$Mass.Loss, outlier.tagging = TRUE)
boxplot(Data2$Mass.Loss, outlier.tagging = TRUE)

plot(Data$PercMassRemaining ~ Data$Day)

ggplot(Data, aes(x = Day, y = PercMassRemaining, color = treatment)) +
  geom_point()
ggplot(Data, aes(x = Day, y = PercMassRemaining, color = Litter.type)) +
  geom_point()



# There appear to be no outliers!

# just for fun...
boxplot(Data$Mass.Loss ~ Data$Day)
boxplot(Data$Mass.Loss ~ Data$treatment)
boxplot(Data$PercMassRemaining ~ Data$Day)

# creating subsets of the data
Pi1_Pine <- Data2 %>% filter(SiteLitterCombo == "Pi1.Pine")
Pi1_Morella <- Data2 %>% filter(SiteLitterCombo == "Pi1.Morella")
All_Pinus_trtmnt <- Data2 %>% filter(treatment == "Pine")
All_Morella_trtmnt <- Data2 %>% filter(treatment == "Morella")
All_Phrag_trtmnt <- Data2 %>% filter(treatment == "Phragmites")

# more subsets and manually finding alpha values
Pi1_Phrag <- Data %>% filter(SiteLitterCombo == "Pi1.Phragmites")
Pi2_Pine <- Data %>% filter(SiteLitterCombo == "Pi2.Pine")
Pi2_Phrag <- Data %>% filter(SiteLitterCombo == "Pi2.Phragmites")
Pi2_Morella <- Data %>% filter(SiteLitterCombo == "Pi2.Morella")
Pi3_Pine <- Data %>% filter(SiteLitterCombo == "Pi3.Pine")
Pi3_Phrag <- Data %>% filter(SiteLitterCombo == "Pi3.Phragmites")
Pi3_Morella <- Data %>% filter(SiteLitterCombo == "Pi3.Morella")
Mo1_Pine <- Data %>% filter(SiteLitterCombo == "Mo1.Pine")
Mo1_Phrag <- Data %>% filter(SiteLitterCombo == "Mo1.Phragmites")
Mo1_Morella <- Data %>% filter(SiteLitterCombo == "Mo1.Morella")
Mo2_Pine <- Data %>% filter(SiteLitterCombo == "Mo2.Pine")
Mo2_Phrag <- Data %>% filter(SiteLitterCombo == "Mo2.Phragmites")
Mo2_Morella <- Data %>% filter(SiteLitterCombo == "Mo2.Morella")
Mo3_Pine <- Data %>% filter(SiteLitterCombo == "Mo3.Pine")
Mo3_Phrag <- Data %>% filter(SiteLitterCombo == "Mo3.Phragmites")
Mo3_Morella <- Data %>% filter(SiteLitterCombo == "Mo3.Morella")
Ph1_Pine <- Data %>% filter(SiteLitterCombo == "Ph1.Pine")
Ph1_Phrag <- Data %>% filter(SiteLitterCombo == "Ph1.Phragmites")
Ph1_Morella <- Data %>% filter(SiteLitterCombo == "Ph1.Morella")
Ph2_Pine <- Data %>% filter(SiteLitterCombo == "Ph2.Pine")
Ph2_Phrag <- Data %>% filter(SiteLitterCombo == "Ph2.Phragmites")
Ph2_Morella <- Data %>% filter(SiteLitterCombo == "Ph2.Morella")
Ph3_Pine <- Data %>% filter(SiteLitterCombo == "Ph3.Pine")
Ph3_Phrag <- Data %>% filter(SiteLitterCombo == "Ph3.Phragmites")
Ph3_Morella <- Data %>% filter(SiteLitterCombo == "Ph3.Morella")


plot(Pi1_Pine$Day, Pi1_Pine$PercMassRemaining)
curve(x^-0.006, from = 0, to = 365, add = TRUE, col = "violet")

plot(Pi1_Pine$Day, Pi1_Pine$Dry_Mass)
curve(x^-0.45, from = 0, to = 365, add = TRUE, col = "violet")

plot(Pi1_Phrag$Day, Pi1_Phrag$PercMassRemaining)
curve(x^-0.006, from = 0, to = 365, add = TRUE, col = "violet")

plot(Pi1_Morella$Day, Pi1_Mor$PercMassRemaining)
curve(x^-0.006, from = 0, to = 365, add = TRUE, col = "violet")

plot(Mo1_Pine$Day, Mo1_Pine$PercMassRemaining)
curve(x^-0.009, from = 0, to = 365, add = TRUE, col = "violet")

plot(Ph3_Morella$Day, Ph3_Morella$PercMassRemaining, ylim = c(0.8, 1))
curve(x^-0.012, from = 0, to = 365, add = TRUE, col = "violet", lwd = 8)

# setting up for fit curves: copied from internet

nls(PercMassRemaining ~ PercMassRemaining + (PercMassInitial - PercMassRemaining) * exp(-alpha * Day),
  data = Data2,
  start = list(y0 = 1, yf = 0.96, alpha = 0.008)
)


fit <- nls(PercMassRemaining ~ SSasymp(Day, log_alpha), data = All_Pinus_trtmnt)
trial <- SSasymp(Pi1_Pine$Day, 365, 0.75, 0.1)

get_alpha <- function(pmr, t) -log(pmr) / t



# we want to model decline in mass as a function of time


# Trying to skip ahead and do it iteratively... but it's still not working.
fitted <- Data %>%
  nest(SiteLitterCombo) %>%
  mutate(
    fit = map(Data, ~ nls(PercMassRemaining ~ SSasymp(Day, PercMassRemaining, PercMassInitial, log_alpha))),
    tidied = map(fit, tidy),
    augmented = map(fit, augment),
  )

# Produce a table of fit parameters: y0, yf, alpha
fitted %>%
  unnest(tidied) %>%
  select(sensor, term, estimate) %>%
  spread(term, estimate) %>%
  mutate(alpha = exp(log_alpha))
