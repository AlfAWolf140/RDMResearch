library(patchwork)
library(tidyverse)
library(readr)
library(ggplot2)

RDMPolicy <- read_csv("Policies.csv")
View(RDMPolicy)


#Strength of DMP requirement barplot
RDMPolicy$DMP <- as.factor(RDMPolicy$DMP)
RDMPolicy$DMP
levels(RDMPolicy$DMP)<-c('No mention', 'If required by funder', 'Encouraged', 'Required', 'Logged or signed off')

g1 <- ggplot(RDMPolicy, aes(DMP)) +
  geom_boxplot(aes(x=RDMPolicy$DMP,y=RDMPolicy$Date)) + 
  scale_y_continuous(limits=c(2011, 2024)) +
  scale_x_discrete(labels=c('No mention', 'If required by funder', 'Encouraged', 'Required', 'Logged or signed off'), drop = FALSE)+
  labs(title="Data Management Plan",
       x="DMP requirement",
       y="Date") +
    theme_bw() +
    theme(panel.grid = element_blank(), axis.title.x = element_blank())

#Strength of data deposit requirement barplot
RDMPolicy$'Data deposit' <- as.factor(RDMPolicy$`Data deposit`)
RDMPolicy$'Data deposit'
levels(RDMPolicy$'Data deposit')<-c('No mention', 'Mentioned', 'Encouraged', 'Required', 'Checked')

g2 <- ggplot(RDMPolicy, aes('Data deposit')) +
  geom_boxplot(aes(x=RDMPolicy$'Data deposit',y=RDMPolicy$Date)) + 
  scale_y_continuous(limits=c(2011, 2024)) +
  scale_x_discrete(labels=c('No mention', 'Mentioned', 'Encouraged', 'Required', 'Checked'), drop = FALSE) +
  labs(title="Deposit in a repository",
       x="Data deposit",
       y="Date") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank())

#Minimum length of time to keep data
RDMPolicy$`Min Time` <- as.factor(RDMPolicy$`Min Time`)
RDMPolicy$`Min Time`
levels(RDMPolicy$`Min Time`)<-c('No mention',	'In accordance with policy',	'<5 years',	'5>= years', 'Indefintely')

g3 <- ggplot(RDMPolicy, aes('Min Time')) +
  geom_boxplot(aes(x=RDMPolicy$'Min Time',y=RDMPolicy$Date)) + 
  scale_y_continuous(limits=c(2011, 2024)) +
  scale_x_discrete(labels=c('No mention',	'In accordance with policy',	'<5 years',	'5>= years', 'Indefintely'), drop = FALSE) +
  labs(title="Minimum length of time to keep data",
       x="Minimum length",
       y="Date") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank())

#Open Licenses
RDMPolicy$`Data licenses` <- as.factor(RDMPolicy$`Data licenses`)
RDMPolicy$`Data licenses`
levels(RDMPolicy$`Data licenses`)<-c('No mention', 'Do not give away rights', 'Any license', 'Open license', 'Only CC-BY or similar')

g4 <- ggplot(RDMPolicy, aes('Data licenses')) +
  geom_boxplot(aes(x=RDMPolicy$'Data licenses',y=RDMPolicy$Date)) + 
  scale_y_continuous(limits=c(2011, 2024)) +
  scale_x_discrete(labels=c('No mention', 'Do not give away rights', 'Any license', 'Open license', 'Only CC-BY or similar'), drop = FALSE) +
  labs(title="Data Licensing",
       x="Data License",
       y="Date") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank())
g4
#Software deposit
RDMPolicy$`Software deposit` <- as.factor(RDMPolicy$`Software deposit`)
RDMPolicy$`Software deposit`
levels(RDMPolicy$`Software deposit`)<-c('No mention',	'Included as data',	'Encouraged',	'Required', 'Repository snapshot')

g5 <- ggplot(RDMPolicy, aes('Software deposit')) +
  geom_boxplot(aes(x=RDMPolicy$'Software deposit',y=RDMPolicy$Date)) + 
  scale_y_continuous(limits=c(2011, 2024)) +
  scale_x_discrete(labels=c('No mention',	'Included as data',	'Encouraged',	'Required', 'Repository snapshot'), drop = FALSE) +
  labs(title="Depositing Software in a repository",
       x="Software deposit",
       y="Date") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank())

#Timeframe for depositing
RDMPolicy$`Deposit timeframe` <- as.factor(RDMPolicy$`Deposit timeframe`)
RDMPolicy$`Deposit timeframe`
levels(RDMPolicy$`Deposit timeframe`)<-c('No mention', 'Minimum delay', 'Publication (with embargo)', 'Publication', 'Collection/As soon as possible')

g6 <- ggplot(RDMPolicy, aes('Deposit timeframe')) +
  geom_boxplot(aes(x=RDMPolicy$'Deposit timeframe',y=RDMPolicy$Date)) + 
  scale_y_continuous(limits=c(2011, 2024)) +
  scale_x_discrete(labels=c('No mention', 'Minimum delay', 'Publication (with embargo)', 'Publication', 'Collection/As soon as possible'), drop = FALSE) +
  labs(title="Deposit timeframe",
       x="Deposit timeframe",
       y="Date") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank())

#Data Availability Requirement

RDMPolicy$`DAS` <- as.factor(RDMPolicy$`DAS`)
RDMPolicy$`DAS`
levels(RDMPolicy$`DAS`)<-c('No mention', 'Mentioned', 'Encouraged', 'Required', 'with DOI')

g7 <- ggplot(RDMPolicy, aes('DAS')) +
  geom_boxplot(aes(x=RDMPolicy$'DAS',y=RDMPolicy$Date)) + 
  scale_y_continuous(limits=c(2011, 2024)) +
  scale_x_discrete(labels=c('No mention', 'Mentioned', 'Encouraged', 'Required', 'with DOI'), drop = FALSE) +
  labs(title="Data Availability Statement",
       x="Data Availability Statement",
       y="Date") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank())
g7
#Data Catalogue
RDMPolicy$`Data catalogue` <- as.factor(RDMPolicy$`Data catalogue`)
RDMPolicy$`Data catalogue`
levels(RDMPolicy$`Data catalogue`)<-c('No mention',	'Mentioned',	'Encouraged',	'Required',	'On collection'
)

g8 <- ggplot(RDMPolicy, aes('Data catalogue')) +
  geom_boxplot(aes(x=RDMPolicy$'Data catalogue',y=RDMPolicy$Date)) + 
  scale_y_continuous(limits=c(2011, 2024)) +
  scale_x_discrete(labels=c('No mention',	'Mentioned',	'Encouraged',	'Required',	'On collection'), drop = FALSE) +
  labs(title="Creation of record in data catalogue",
       x="Data Catalogue",
       y="Date") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank())

#Scope of who the policy applies to
RDMPolicy$Who <- as.factor(RDMPolicy$Who)
RDMPolicy$Who
levels(RDMPolicy$Who)<-c('No mention','PI only',	'All Staff',	'Staff & PGRs',	'Everybody'
)

g9 <- ggplot(RDMPolicy, aes('Who')) +
  geom_boxplot(aes(x=RDMPolicy$'Who',y=RDMPolicy$Date)) + 
  scale_y_continuous(limits=c(2011, 2024)) +
  scale_x_discrete(labels=c('No mention','PI only',	'All Staff',	'Staff & PGRs',	'Everybody'), drop = FALSE) +
  labs(title="Who does the policy apply to?",
       x="Scope",
       y="Date") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank())

g9
#Setup grid
(g1 | g2 | g3) /
(g4 | g5 | g6) /
(g7 | g8 | g9)
