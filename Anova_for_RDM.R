
library(readr)
library(rstatix)

RDMPolicy <- read_csv("PoliciesData_UK_2024.csv")
View(RDMPolicy)


linmod<-RDMPolicy %>%
  filter(Date != "NA") %>%
  lm(Date ~ DMP, data = .)

DMPAnova<- RDMPolicy %>% anova_test(Date ~ DMP)

linmod<-RDMPolicy %>%
  filter(Date != "NA") %>%
  lm(Date ~ `Data deposit`, data = .)
DepositAnova<- RDMPolicy %>% anova_test(Date ~ `Data deposit`)

linmod<-RDMPolicy %>%
  filter(Date != "NA") %>%
  lm(Date ~ `Min Time`, data = .)
MinTimeAnova<- RDMPolicy %>% anova_test(Date ~ `Min Time`)

linmod<-RDMPolicy %>%
  filter(Date != "NA") %>%
  lm(Date ~ `Data licenses`, data = .)
LicensesAnova<- RDMPolicy %>% anova_test(Date ~ `Data licenses`)

linmod<-RDMPolicy %>%
  filter(Date != "NA") %>%
  lm(Date ~ `Software deposit`, data = .)
SoftwareAnova<- RDMPolicy %>% anova_test(Date ~ `Software deposit`)

linmod<-RDMPolicy %>%
  filter(Date != "NA") %>%
  lm(Date ~ `Deposit timeframe`, data = .)
TimeframeAnova<- RDMPolicy %>% anova_test(Date ~ `Deposit timeframe`)

linmod<-RDMPolicy %>%
  filter(Date != "NA") %>%
  lm(Date ~ DAS, data = .)
DASAnova<- RDMPolicy %>% anova_test(Date ~ `DAS`)

linmod<-RDMPolicy %>%
  filter(Date != "NA") %>%
  lm(Date ~ `Data catalogue`, data = .)
CatalogueAnova<- RDMPolicy %>% anova_test(Date ~ `Data catalogue`)

linmod<-RDMPolicy %>%
  filter(Date != "NA") %>%
  lm(Date ~ Who, data = .)
ScopeAnova<- RDMPolicy %>% anova_test(Date ~ `Who`)

DMPAnova
DepositAnova
MinTimeAnova
LicensesAnova
SoftwareAnova
TimeframeAnova
DASAnova
CatalogueAnova
ScopeAnova

