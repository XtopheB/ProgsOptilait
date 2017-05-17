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



##### ----  Parameters used ----


risk.paral  <- function(year.chosed)
  {
  Myroot <- "D:/progs/Optilait/"
  
  # loading the functions
  source(paste(Myroot,"Progs/NpRiskFunctions3.R", sep="")) 
  
  #First we load the data refined by DataManager.R (and used in the Shny app)
  load(paste(Myroot,"Progs/Shiny/dataobserver/data.work.RData", sep=""))
  
  
  #NpMethod <- "SILVERMAN"
  NpMethod <- "CV"
  
  #Parametre multiplicatif des fenetres à la silverman 
  # ampli.Silver <- 1
  
  #Number of bootstraps for the  test : ref Li, Maasoumi et Racine (2009)
  Nboot <- 9
  
  # Sample reduction for tests
  Psample <- 1
  
  NpAversion7(year.chosed,
               Xvar = c("sau", "eqvltotal", "hasfpirri", 
                                  "concvlr", "region4",
                                  "Tyear", "ETPyear", "DELTAyear"),
               bwmethod = NpMethod,
               ampli = ampli.Silver, 
               samplereduc = Psample,
               bwcompute= "TRUE",
               Myroot = Myroot,
               data = data.work)
}


###-----  Parallelisation ------ ###
nb.cpus=6

yearlist<- c("2002", "2003", "2004", "2005")

## Initialisation
sfInit(parallel=TRUE, cpus=nb.cpus)

toto <-system.time(
  
sfClusterApplyLB(yearlist, risk.paral)

)

## Stop paralelisation
sfStop()



#####----  Significance Tests -----  ######

library(np)

SigProd <- function(year.chosed){
  library(np)
  #Number of bootstraps for the  test : ref Li, Maasoumi et Racine (2009)
  Nboot <- 399
  # Sample reduction for tests
  Psample <- 1
  
  Myroot <- "D:/progs/Optilait/"
  
  #We load the bw object computed (for the whole sample)
  load(paste(Myroot,"Results/bw.f.CV.",year.chosed,"-",Psample,".RData", sep=""))
  
  np.f.sig <- npsigtest(bw.f, boot.num = Nboot)
  print(summary(np.f.sig))
  
  save(np.f.sig,  file = paste(Myroot,"Results/sig.f-",year.chosed,".RData", sep=""))

}

#SigProd(2003)


###-----  Parallelisation ------ ###
nb.cpus=6

yearlist<- c("2002", "2003", "2004", "2005")

## Initialisation
sfInit(parallel=TRUE, cpus=nb.cpus)

toto <-system.time(
  
  sfClusterApplyLB(yearlist, SigProd)
  
)

## Stop paralelisation
sfStop()


####### ----- Specification test -----  ######
# we use vignette(np)



SpeTest <- function(year.chosed){
  library(np)
  Nboot <- 399
  
  Myroot <- "D:/progs/Optilait/"
    
  #First we load the data refined by DataManager.R (and used in the Shny app)
    
  load(paste(Myroot,"Progs/Shiny/dataobserver/data.work.RData", sep=""))
  data.year <- subset(data.work, year == year.chosed)
  
  Model.ols <- lm(laitproduit ~ sau + eqvltotal + hasfpirri +concvlr
                  + region4 + Tyear + ETPyear + DELTAyear, 
                  x=TRUE, y = TRUE, data = data.year)
  
  # Creating the data.frame used in regression (used for tests)
  
  X <- data.year[,c("sau", "eqvltotal" ,"hasfpirri", "concvlr",
         "region4" ,"Tyear" , "ETPyear" , "DELTAyear")]

  result.spe <- npcmstest(model = Model.ols, 
                      xdat = X, 
                      ydat = data.year$laitproduit,
                      boot.num = Nboot,
                      nmulti = 1,
                      tol = 0.1,
                      ftol = 0.1)
  
  
  print(result.spe)
  
  save(result.spe,  file = paste(Myroot,"Results/spe.f-",year.chosed,".RData", sep=""))
  
}


# SpeTest(2003)
###-----  Parallelisation ------ ###
nb.cpus=2

yearlist<- c("2002", "2003", "2004", "2005")

## Initialisation
sfInit(parallel=TRUE, cpus=nb.cpus)

toto <-system.time(
  
  sfClusterApplyLB(yearlist, SpeTest)
  
)

## Stop paralelisation
sfStop()

