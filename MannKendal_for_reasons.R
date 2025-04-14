# Install and load the "zyp" package
install.packages("zyp")
install.packages("ggplot2")
library(zyp)
library(ggplot2)
library(scales)
library(readr)
library(tidyverse)
library(patchwork)

ggplot() +
  geom_point(aes(x = RDMPolicy$Date, y = RDMPolicy$Total), color = "blue") +
  labs(x = "Year", y = "Total score") +
  theme_classic()



RDMPolicyReasons <- read_csv("Reasons_proportions.csv")
RDMPolicyReasons2 <- RDMPolicyReasons %>%
  filter(Date >=2015)


# Perform the Mann-Kendall Trend Test
ComplianceMK <- MannKendall(RDMPolicyReasons2$Compliance)
IntegrityMK <- MannKendall(RDMPolicyReasons2$Integrity)
ReuseMK <- MannKendall(RDMPolicyReasons2$Reuse)
ValueMK <- MannKendall(RDMPolicyReasons2$Value)
ExcellenceMK <- MannKendall(RDMPolicyReasons2$Excellence)
PublicMK <- MannKendall(RDMPolicyReasons2$`Public Interest`)
ImpactMK <- MannKendall(RDMPolicyReasons2$Impact)
RisksMK <- MannKendall(RDMPolicyReasons2$Risks)
BenefitsMK <- MannKendall(RDMPolicyReasons2$Benefits)
RecognitionMK <- MannKendall(RDMPolicyReasons2$Recognition)
EthicsMK <- MannKendall(RDMPolicyReasons2$Ethics)


#Print results
ComplianceMK
IntegrityMK
ReuseMK
ValueMK
ExcellenceMK
PublicMK
ImpactMK
RisksMK
BenefitsMK
RecognitionMK
EthicsMK

years <- 2015:2024

# Calculate the trend line
trend_line_ComplianceMK <- predict(loess(RDMPolicyReasons2$Compliance ~ years))
trend_line_ComplianceMK
# Create a trend line plot
graph1 <- ggplot() +
  geom_point(aes(x = years, y = RDMPolicyReasons2$Compliance), color = "blue") +
#  geom_line(aes(x = years, y = trend_line_ComplianceMK), color = "red") +
  labs(x = "Year", y = "Compliance Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()
#graph1

trend_line_IntegrityMK  <- predict(loess(RDMPolicyReasons2$Integrity ~ years))

# Create a trend line plot
graph2<- ggplot() +
  geom_point(aes(x = years, y = RDMPolicyReasons2$Integrity), color = "blue") +
 # geom_line(aes(x = years, y = trend_line_IntegrityMK), color = "red") +
  labs(x = "Year", y = "Integrity Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()

graph2

trend_line_ReuseMK  <- predict(loess(RDMPolicyReasons2$Reuse ~ years))

# Create a trend line plot
graph3<-  ggplot() +
  geom_point(aes(x = years, y = RDMPolicyReasons2$Reuse), color = "blue") +
#  geom_line(aes(x = years, y = trend_line_ReuseMK), color = "red") +
  labs(x = "Year", y = "Reuse Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()
graph3

trend_line_ValueMK  <- predict(loess(RDMPolicyReasons2$Value ~ years))

# Create a trend line plot
graph4<- ggplot() +
  geom_point(aes(x = years, y = RDMPolicyReasons2$Value), color = "blue") +
#  geom_line(aes(x = years, y = trend_line_ValueMK), color = "red") +
  labs(x = "Year", y = "Value Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()
graph4

trend_line_ExcellenceMK   <- predict(loess(RDMPolicyReasons2$Excellence ~ years))

# Create a trend line plot
graph5<- ggplot() +
  geom_point(aes(x = years, y = RDMPolicyReasons2$Excellence), color = "blue") +
#  geom_line(aes(x = years, y = trend_line_ExcellenceMK ), color = "red") +
  labs(x = "Year", y = "Excellence Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()
graph5

trend_line_PublicMK    <- predict(loess(RDMPolicyReasons2$`Public Interest` ~ years))

# Create a trend line plot
graph6<- ggplot() +
  geom_point(aes(x = years, y = RDMPolicyReasons2$`Public Interest`), color = "blue") +
#  geom_line(aes(x = years, y = trend_line_PublicMK  ), color = "red") +
  labs(x = "Year", y = "Public Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()
graph6

trend_line_ImpactMK     <- predict(loess(RDMPolicyReasons2$Impact ~ years))

# Create a trend line plot
graph7<- ggplot() +
  geom_point(aes(x = years, y = RDMPolicyReasons2$Impact), color = "blue") +
#  geom_line(aes(x = years, y = trend_line_ImpactMK), color = "red") +
  labs(x = "Year", y = "Impact Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()
graph7

trend_line_RisksMK     <- predict(loess(RDMPolicyReasons2$Risks ~ years))

# Create a trend line plot
graph8<- ggplot() +
  geom_point(aes(x = years, y = RDMPolicyReasons2$Risks), color = "blue") +
#  geom_line(aes(x = years, y = trend_line_RisksMK), color = "red") +
  labs(x = "Year", y = "Risks Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()
graph8

trend_line_BenefitsMK      <- predict(loess(RDMPolicyReasons2$Benefits ~ years))

# Create a trend line plot
graph9<- ggplot() +
  geom_point(aes(x = years, y = RDMPolicyReasons2$Benefits), color = "blue") +
#  geom_line(aes(x = years, y = trend_line_BenefitsMK ), color = "red") +
  labs(x = "Year", y = "Benefits Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()
graph9

trend_line_RecognitionMK <- predict(loess(RDMPolicyReasons2$Recognition ~ years))

# Create a trend line plot
graph10<- ggplot() +
  geom_point(aes(x = years, y = RDMPolicyReasons2$Recognition), color = "blue") +
#  geom_line(aes(x = years, y = trend_line_RecognitionMK), color = "red") +
  labs(x = "Year", y = "Recognition Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()
graph10

trend_line_EthicsMK <- predict(loess(RDMPolicyReasons2$Ethics ~ years))

# Create a trend line plot
graph11<- ggplot() +
  geom_point(aes(x = years, y = RDMPolicyReasons2$Ethics), color = "blue") +
#  geom_line(aes(x = years, y = trend_line_EthicsMK), color = "red") +
  labs(x = "Year", y = "Ethics Occurance") +
  scale_x_continuous(breaks = seq(from = floor(min(years)), to = ceiling(max(years)), by = 2)) +
  scale_y_continuous(limits = c(0, 105)) +
  theme_classic()
graph11


(graph1 | graph2 | graph3) /
  (graph4 | graph5 | graph6) /  
  (graph7 | graph8 | graph9) /  
  (graph10 | graph11 | graph11) 

