#  ------------  
# DataObserver.R is devoted to dynamic analysis of raw data (after first filtering in Stata)
# 25/04/2017 : Creation from ShinyRisk.R


rm(list=ls())
#install.packages('shiny')
library(shiny)
library(np)
library(foreign)
library(ggplot2)
library(ggthemes)
library(doBy)
library(quantreg)


#Myroot <- "D:/progs/Optilait/"
Myroot  <- "C:/Chris/progs/Optilait/"   


#Loading dataset
data.all <- read.dta(paste(Myroot,"data/OptilaitMerge3.dta", sep=""))    # All years 

# On cylindre data.all 
data.all <- data.all[complete.cases(data.all),]
data.all$concvlr <- as.numeric(data.all$concvlr)

#On definit le fichier de travail
data.work <- subset(data.all, 
                    select = c(annee, ident, prixconcvlr, prixlaiteriemoyen,
                               laitproduit, sau, sfp, eqvltotal,
                               hasfpirri, charpot, quotalait, concvlr,region,
                               Profit,SigmaProf, 
                               Tyear,ETPyear, DELTAyear,
                               Tprod,ETPprod, DELTAprod) )

data.work <- data.work[complete.cases(data.work),]
data.work$region <- factor(data.work$region)

data.work$year <- factor(data.work$annee)

# Some checks
nrow(data.work)
length(unique(data.work$ident))
length(unique(data.work$annee))

#Saving the file for shiny app
save(data.work,  file = paste(Myroot,"Data/data.work.RData", sep=""))





#Shiny version in subdirectory 
setwd(paste(Myroot,"Progs/Shiny", sep=""))
load("DataObserver/data.work.RData", envir = .GlobalEnv)


#attach(risk.all)
runApp("DataObserver")
