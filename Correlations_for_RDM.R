install.packages("jmuOutlier")

library(readr)
library(jmuOutlier)

RDMPolicy <- read_csv("Policies.csv")
View(RDMPolicy)
#RDMPolicy = RDMPolicy[-61,] 
#View(RDMPolicy)


DMPCor <- cor(RDMPolicy$Date, RDMPolicy$DMP, method='spearman')
DepositCor <- cor(RDMPolicy$Date, RDMPolicy$`Data deposit`, method='spearman')
MinTimeCor <- cor(RDMPolicy$Date, RDMPolicy$`Min Time`, method='spearman')
LicenseCor <- cor(RDMPolicy$Date, RDMPolicy$`Data licenses`, method='spearman')
SoftwareCor <- cor(RDMPolicy$Date, RDMPolicy$`Software deposit`, method='spearman')
TimeframeCor <- cor(RDMPolicy$Date, RDMPolicy$`Deposit timeframe`, method='spearman')
DASCor <- cor(RDMPolicy$Date, RDMPolicy$DAS, method='spearman')
CatalogueCor <- cor(RDMPolicy$Date, RDMPolicy$`Data catalogue`, method='spearman')
ScopeCor <- cor(RDMPolicy$Date, RDMPolicy$Who, method='spearman')

perm.cor.test(RDMPolicy$Date, RDMPolicy$DMP, "t", "spearman" )
perm.cor.test(RDMPolicy$Date, RDMPolicy$`Data deposit`, "t", "spearman" )
perm.cor.test(RDMPolicy$Date, RDMPolicy$`Min Time`, "t", "spearman" )
perm.cor.test(RDMPolicy$Date, RDMPolicy$`Data licenses`, "t", "spearman" )
perm.cor.test(RDMPolicy$Date, RDMPolicy$`Software deposit`, "t", "spearman" )
perm.cor.test(RDMPolicy$Date, RDMPolicy$`Deposit timeframe`, "t", "spearman" )
perm.cor.test(RDMPolicy$Date, RDMPolicy$DAS, "t", "spearman" )
perm.cor.test(RDMPolicy$Date, RDMPolicy$`Data catalogue`, "t", "spearman" )
perm.cor.test(RDMPolicy$Date, RDMPolicy$Who, "t", "spearman" )

DMPCor
DepositCor
MinTimeCor
LicenseCor
SoftwareCor
TimeframeCor
DASCor
CatalogueCor
ScopeCor
