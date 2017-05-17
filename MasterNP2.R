# Programme maitre de génération de documents multiples (Sweave)
# 29/08/2013 : Paralelisation et simplification du processus (from Master.R)
# 02/09/2013 : Création fonction d'affectation des dossiers (Thibault)
# 02/11/2015 : Adaptation to New Companion.Rnx files 

# Remove all 
rm(list=ls())

## Second change the working directory

setwd("D:/progs/Optilait/")  



library(foreign)
library("snowfall")
library(np)


##### ----  Parameters used ----



Myroot <- "D:/progs/Optilait/"

# loading the functions
source(paste(Myroot,"Progs/NpRiskFunctions3.R", sep="")) 

#First we load the data refined by DataManager.R (and used in the Shny app)
load(paste(Myroot,"Progs/Shiny/dataobserver/data.work.RData", sep=""))


#NpMethod <- "SILVERMAN"
NpMethod <- "CV"

#Parametre multiplicatif des fenetres à la silverman 
ampli.Silver <- 1

#Number of bootstraps for the  test : ref Li, Maasoumi et Racine (2009)
Nboot<- 9

# Sample reduction for tests
Psample <- 0.01


### Parallelisation 
nb.cpus=6

yearlist<- c("2002", "2003", "2004", "2005")

sfInit(parallel=TRUE, cpus=nb.cpus)

toto <-system.time(
  
sfClusterApplyLB(yearlist, NpAversion6,
                                       Xvar = c("sau", "eqvltotal", "hasfpirri", 
                                                "concvlr", "region4",
                                                "Tyear", "ETPyear", "DELTAyear"),
                                       bwmethod = NpMethod,
                                       ampli = ampli.Silver, 
                                       samplereduc = Psample,
                                       bwcompute= "TRUE",
                                       Myroot = Myroot,
                                       data = data.work)

)
# arret de la paralelisation
sfStop()









