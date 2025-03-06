# Install and load the "zyp" package
install.packages("zyp")
install.packages("ggplot2")
library(zyp)
library(ggplot2)
library(scales)
library(readr)
library(tidyverse)

RDMPolicyAdj <- read_csv("Adj_proportions.csv")
RDMPolicyAdj2 <- RDMPolicyAdj %>%
  filter(Date >=2015)

# Perform the Mann-Kendall Trend Test
OpenMK <- MannKendall(RDMPolicyAdj2$Open)
DiscoverableMK <- MannKendall(RDMPolicyAdj2$Discoverable)
FindableMK <- MannKendall(RDMPolicyAdj2$Findable)
AccessibleMK <- MannKendall(RDMPolicyAdj2$Accessible)
AvailableMK <- MannKendall(RDMPolicyAdj2$Available)
AccurateMK <- MannKendall(RDMPolicyAdj2$Accurate)
CompleteMK <- MannKendall(RDMPolicyAdj2$Complete)
AuthenticMK <- MannKendall(RDMPolicyAdj2$Authentic)
ReliableMK <- MannKendall(RDMPolicyAdj2$Reliable)
ValuableMK <- MannKendall(RDMPolicyAdj2$Valuable)
IdentifiableMK <- MannKendall(RDMPolicyAdj2$Identifiable)
RetrievableMK <- MannKendall(RDMPolicyAdj2$Retrievable)
TimelyMK <- MannKendall(RDMPolicyAdj2$Timely)
FAIRMK <- MannKendall(RDMPolicyAdj2$FAIR)
ReuseableMK <- MannKendall(RDMPolicyAdj2$Reuseable)
InteroperableMK <- MannKendall(RDMPolicyAdj2$Interoperable)

#Print results
OpenMK
DiscoverableMK 
FindableMK
AccessibleMK
AvailableMK 
AccurateMK 
CompleteMK 
AuthenticMK 
ReliableMK 
ValuableMK
IdentifiableMK
RetrievableMK 
TimelyMK 
FAIRMK 
ReuseableMK 
InteroperableMK

years <- 2015:2024

# Calculate the trend line
trend_line_OpenMK <- predict(loess(RDMPolicyAdj2$Open ~ years))
trend_line_OpenMK
# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Open), color = "blue") +
  geom_line(aes(x = years, y = trend_line_OpenMK), color = "red") +
  labs(x = "Year", y = "Open Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()


trend_line_DiscoverableMK  <- predict(loess(RDMPolicyAdj2$Discoverable ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Discoverable), color = "blue") +
  geom_line(aes(x = years, y = trend_line_DiscoverableMK), color = "red") +
  labs(x = "Year", y = "Discoverable Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()


trend_line_FindableMK  <- predict(loess(RDMPolicyAdj2$Findable ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Findable), color = "blue") +
  geom_line(aes(x = years, y = trend_line_FindableMK), color = "red") +
  labs(x = "Year", y = "Findable Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()


trend_line_AccessibleMK  <- predict(loess(RDMPolicyAdj2$Accessible ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Accessible), color = "blue") +
  geom_line(aes(x = years, y = trend_line_AccessibleMK), color = "red") +
  labs(x = "Year", y = "Accessible Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()


trend_line_AvailableMK   <- predict(loess(RDMPolicyAdj2$Available ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Available), color = "blue") +
  geom_line(aes(x = years, y = trend_line_AvailableMK ), color = "red") +
  labs(x = "Year", y = "Available Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()

trend_line_AccurateMK    <- predict(loess(RDMPolicyAdj2$Accurate ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Accurate), color = "blue") +
  geom_line(aes(x = years, y = trend_line_AccurateMK  ), color = "red") +
  labs(x = "Year", y = "Accurate Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()


trend_line_CompleteMK     <- predict(loess(RDMPolicyAdj2$Complete ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Complete), color = "blue") +
  geom_line(aes(x = years, y = trend_line_CompleteMK), color = "red") +
  labs(x = "Year", y = "Complete Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()

 
trend_line_AuthenticMK     <- predict(loess(RDMPolicyAdj2$Authentic ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Authentic), color = "blue") +
  geom_line(aes(x = years, y = trend_line_AuthenticMK), color = "red") +
  labs(x = "Year", y = "Authentic Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()


trend_line_ReliableMK      <- predict(loess(RDMPolicyAdj2$Reliable ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Reliable), color = "blue") +
  geom_line(aes(x = years, y = trend_line_ReliableMK ), color = "red") +
  labs(x = "Year", y = "Reliable Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()


trend_line_ValuableMK <- predict(loess(RDMPolicyAdj2$Valuable ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Valuable), color = "blue") +
  geom_line(aes(x = years, y = trend_line_ValuableMK), color = "red") +
  labs(x = "Year", y = "Valuable Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()


trend_line_IdentifiableMK <- predict(loess(RDMPolicyAdj2$Identifiable ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Identifiable), color = "blue") +
  geom_line(aes(x = years, y = trend_line_IdentifiableMK), color = "red") +
  labs(x = "Year", y = "Identifiable Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()

trend_line_RetrievableMK  <- predict(loess(RDMPolicyAdj2$Retrievable ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Retrievable), color = "blue") +
  geom_line(aes(x = years, y = trend_line_RetrievableMK ), color = "red") +
  labs(x = "Year", y = "Retrievable Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()


trend_line_TimelyMK  <- predict(loess(RDMPolicyAdj2$Timely ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Timely), color = "blue") +
  geom_line(aes(x = years, y = trend_line_TimelyMK  ), color = "red") +
  labs(x = "Year", y = "Timely Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()


trend_line_FAIRMK   <- predict(loess(RDMPolicyAdj2$FAIR ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$FAIR), color = "blue") +
  geom_line(aes(x = years, y = trend_line_FAIRMK  ), color = "red") +
  labs(x = "Year", y = "FAIR Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()


trend_line_ReuseableMK    <- predict(loess(RDMPolicyAdj2$Reuseable ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Reuseable), color = "blue") +
  geom_line(aes(x = years, y = trend_line_ReuseableMK ), color = "red") +
  labs(x = "Year", y = "Reuseable Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()


trend_line_InteroperableMK    <- predict(loess(RDMPolicyAdj2$Interoperable ~ years))

# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = RDMPolicyAdj2$Interoperable), color = "blue") +
  geom_line(aes(x = years, y = trend_line_InteroperableMK), color = "red") +
  labs(x = "Year", y = "Interoperable Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()




# Create a trend line plot
ggplot() +
  geom_point(aes(x = years, y = adj), color = "blue") +
  geom_line(aes(x = years, y = trend_line_OpenMK), color = "red") +
  labs(x = "Year", y = "Recognition Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 1), labels = label_percent()) +
  theme_classic()
