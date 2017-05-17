library(DataExplorer)

load("data.work.RData")
# faire sur nos variables

CorrelationContinuous(data.work, use = "na.or.complete")

PlotMissing(data.work)



toto <- subset(data.work, region == 2)
titi <-unique(toto$Tyear)


 