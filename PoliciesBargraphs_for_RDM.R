library(patchwork)
library(tidyverse)
library(readr)
library(ggplot2)

RDMPolicy <- read_csv("PoliciesData_UK_2024.csv")
View(RDMPolicy)

#Strength of DMP requirement barplot
RDMPolicy$DMP <- as.factor(RDMPolicy$DMP)
RDMPolicy$DMP
levels(RDMPolicy$DMP)<-c('No mention', 'If required by funder', 'Encouraged', 'Required', 'Logged or signed off')

g1<-ggplot(RDMPolicy, aes(DMP)) + 
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count)))) + 
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  scale_x_discrete(labels=c('No mention', 'If required by funder', 'Encouraged', 'Required', 'Logged or signed off'))	+
  labs(title="Data Management Plan",
       x="DMP requirement",
       y="Percent") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank())

#Strength of data deposit requirement barplot
RDMPolicy$'Data deposit' <- as.factor(RDMPolicy$`Data deposit`)
RDMPolicy$'Data deposit'
levels(RDMPolicy$'Data deposit')<-c('No mention', 'Mentioned', 'Encouraged', 'Required', 'Checked')

g2<-ggplot(RDMPolicy, aes(`Data deposit`)) + 
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count)))) + 
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  scale_x_discrete(labels=c('No mention', 'Mentioned', 'Encouraged', 'Required', 'Checked'))	+
  labs(title="Deposit in a repository",
       x="Repository deposit",
       y="Percent") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank())

#Minimum length of time to keep data
RDMPolicy$`Min Time` <- as.factor(RDMPolicy$`Min Time`)
RDMPolicy$`Min Time`
levels(RDMPolicy$`Min Time`)<-c('No mention',	'In accordance with policy',	'<5 years',	'5>= years', 'Indefintely')

g3<-ggplot(RDMPolicy, aes(`Min Time`)) + 
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count)))) + 
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  scale_x_discrete(labels=c('No mention',	'In accordance with policy', '<5 years', '5>= years', 'Indefinitely'))	+
  labs(title="Minimum length of time to keep data",
       x="Time",
       y="Percent") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank())

#Open Licenses
RDMPolicy$`Data licenses` <- as.factor(RDMPolicy$`Data licenses`)
RDMPolicy$`Data licenses`
levels(RDMPolicy$`Data licenses`)<-c('No mention', 'Do not give away rights', 'Any license', 'Open license', 'Only CC-BY or similar')

g4<-ggplot(RDMPolicy, aes(`Data licenses`)) + 
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count)))) + 
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  scale_x_discrete(labels=c('No mention', 'Do not give away rights', 'Any license', 'Open license', 'Only CC-BY or similar'))	+
  labs(title="Data Licensing",
       x="License",
       y="Percent") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank())

#Software deposit
RDMPolicy$`Software deposit` <- as.factor(RDMPolicy$`Software deposit`)
RDMPolicy$`Software deposit`
levels(RDMPolicy$`Software deposit`)<-c('No mention',	'Included as data',	'Encouraged',	'Required', 'Repository snapshot')

g5<-ggplot(RDMPolicy, aes(`Software deposit`)) + 
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count)))) + 
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  scale_x_discrete(labels=c('No mention',	'Included as data',	'Encouraged',	'Required', 'Repository snapshot'))	+
  labs(title="Depositing Software in a repository",
       y="Percent") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank())

#Timeframe for depositing
RDMPolicy$`Deposit timeframe` <- as.factor(RDMPolicy$`Deposit timeframe`)
RDMPolicy$`Deposit timeframe`
levels(RDMPolicy$`Deposit timeframe`)<-c('No mention', 'Minimum delay', 'Publication (with embargo)', 'Publication', 'Collection/As soon as possible'
)

g6<-ggplot(RDMPolicy, aes(`Deposit timeframe`)) + 
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count)))) + 
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  scale_x_discrete(labels=c('No mention', 'Minimum delay', 'Publication (with embargo)', 'Publication', 'Collection/ASAP'))	+
  labs(title="Deposit timeframe",
       x="Policy",
       y="Percent") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank())

#Data Availability Requirement

RDMPolicy$`DAS` <- as.factor(RDMPolicy$`DAS`)
RDMPolicy$`DAS`
levels(RDMPolicy$`DAS`)<-c('No mention', 'Encouraged', 'Required', 'with DOI', 'Contains link'
)

g7<-ggplot(RDMPolicy, aes(`DAS`)) + 
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count)))) + 
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  scale_x_discrete(labels=c('No mention', 'Encouraged', 'Required', 'with DOI', 'Contains link'))	+
  labs(title="Data Availability Statement",
       x="`Policy`",
       y="Percent") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank())

#Data Catalogue
RDMPolicy$`Data catalogue` <- as.factor(RDMPolicy$`Data catalogue`)
RDMPolicy$`Data catalogue`
levels(RDMPolicy$`Data catalogue`)<-c('No mention',	'Mentioned',	'Encouraged',	'Required',	'On collection'
                                      
)

g8<-ggplot(RDMPolicy, aes(`Data catalogue`)) + 
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count)))) + 
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  scale_x_discrete(labels=c('No mention',	'Mentioned',	'Encouraged',	'Required',	'On collection'))	+
  labs(title="Creation of record in a Data Catalogue",
       x="`Policy`",
       y="Percent") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank())

#Scope of who the policy applies to
RDMPolicy$Who <- as.factor(RDMPolicy$Who)
RDMPolicy$Who
levels(RDMPolicy$Who)<-c('No mention','PI only',	'All Staff',	'Staff & PGRs',	'Everybody'
                         
)

g9<-ggplot(RDMPolicy, aes(Who)) + 
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count)))) + 
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  scale_x_discrete(labels=c('No mention',	'PI only', 'All Staff', 'Staff & PGRs',	'Everybody'))	+
  labs(title="Who does the policy apply to?",
       x="`Who`",
       y="Percent") +
  theme_bw() +
  theme(panel.grid = element_blank(), axis.title.x = element_blank())
#Create a 3 x 3 plot of graphs. Layout still needs work.

(g1 | g2 | g3) /
(g4 | g5 | g6) /
(g7 | g8 | g9)
