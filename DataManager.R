
library(DataExplorer)
library(foreign)

#library(plyr)
library(dplyr)
library(lattice)
library(ggplot2)
library(Hmisc)
#library(latticeExtra)

rm(list=ls())

Myroot <- "D:/progs/Optilait/"
# #Myroot  <- "C:/Chris/progs/Optilait/"   

# We use the operation made in the latest CompanionNew.Rnw file (03/2016)
# File already merged with weather data (Corrected file 15/02/2016)

data.all <- read.dta(paste(Myroot,"data/OptilaitMerge3.dta", sep=""))    # All years 

# # On cylindre data.all et on formate comme il faut 
# data.all <- data.all[complete.cases(data.all),]
data.all$concvlr <- as.numeric(data.all$concvlr)
data.all$year  <- factor(data.all$annee)
data.all$region <- factor(data.all$region)


#Refining Data set 
data.work <- subset(data.all, 
                    select = c(annee, ident, prixconcvlr, prixlaiteriemoyen,
                               laitproduit, sau, eqvltotal,
                               hasfpirri,  quotalait, concvlr,region,
                               Profit, Tyear,ETPyear, DELTAyear, year) )

N <- nrow(data.work)

#### Regrouping regions 1, 3 & 5 into "1"  (Stephane 28/04/2017)
data.work$region4 <- car::recode(data.work$region, "'3' = '1'; '5' = '1'; '6' = '3' ")

#Restricting Sample to 5 years (Stephane 28/04/2017)
data.work <- subset(data.work,  annee >= 2002 & annee <= 2005)

#### defining new variables (profit)

data.work <- data.work %>%
  mutate(Profit = prixlaiteriemoyen * laitproduit/1000
                    - prixconcvlr * (concvlr * eqvltotal)/1000 
                    - 15  * hasfpirri )

###  Cylindrer on those years !!! 

data.work <- data.work %>%
  group_by(ident) %>%
  mutate(
    Nb_years =  n_distinct(year)
  )

#View(subset(data.work, Nb_years !=4))
n_distinct(subset(data.work, Nb_years !=4)$ident )

data.work <- subset(data.work, Nb_years ==4)


##### -----  Outlier Detection ----- 

# Loading my ploting functions 
source(paste(Myroot,"Progs/MyToys.R", sep=""))

#serie of plots for outlier detection 
MyPlots(data.work, eqvltotal, laitproduit, choix = 2 )  # 0K
MyPlots(subset(data.work, hasfpirri >0 ),  sau, hasfpirri, choix = 2)
MyPlots(data.work, sau, laitproduit,  choix = 2)



## #-- Detection of outliers and other data problems 
# --- SAU   (CORRECTED 4/05/2017)

# We have 2 points with huge SAU --> mean of 2002 -2004

#boxplot(data.work$sau)
#View(subset(data.work, sau > 800) )

#  Changer SAU pour : 81048069 en 2005 (erreur de virgule)
#          SAU pour : 48029039 en 2003  (idem)
#          SAU pour 81178048 celle de  années passées 

#These guys have a unit pb (997 instead of 99.7)
data.work$sau <- ifelse(data.work$sau > 800 & data.work$ident != 81178048 , data.work$sau/10 , data.work$sau) 
data.work$sau[data.work$ident == 81178048] <- data.all$sau[data.all$ident==81178048 & data.all$annee == 2000]

## Pb of missing value for a regular guy: We impute value of past year (2004)
View(subset(data.work, ident == 12136138))
data.work$sau[which(data.work$ident == 12136138 & data.work$year == 2005)] <- data.all$sau[which(data.all$ident == 12136138 & data.all$year == 2004)] 

#View(subset(data.work, ident == 81178048))
#boxplot(data.work$sau)

### --- concvlr  

MyPlots(data.work, eqvltotal, concvlr,  choix = 2)

# View(subset(data.work,  concvlr >5000))
print(subset(data.work,  ident == 64259038, select = c("ident", "annee", "concvlr", "prixconcvlr", "Profit", "sau", "eqvltotal") ))
# Ident  64484034 n'est pas présent en 2005 (devrait ne plus apparaitre ici)
# Ident == 64259038  a stoké en 2004 avec un prix qui est une erreur 

data.work$prixconcvlr[data.work$ident == 64259038 & data.work$year ==2004] <- 1000 * data.all$prixconcvlr[data.all$ident == 64259038 & data.all$year ==2004]

### ---   Prix conclvlr 
# boxplot(data.work$prixconcvlr)
# MyPlots(data.work, concvlr,  prixconcvlr, choix = 2)
# View(subset(data.work, prixconcvlr < 10 ))

###  Changer prixconclr avec prix moyen individuel 

data.work$prixconcvlr <- ifelse(data.work$prixconcvlr < 10, NA , data.work$prixconcvlr) 
# ii <- is.na(data.work$prixconcvlr)
# View(data.work[ii,])

#Create mean price 
data.work <- data.work %>%
  group_by(ident) %>%
  mutate(
    New_price =  mean(prixconcvlr, na.rm=TRUE)
  )
# Affect new_price to NAs
data.work$prixconcvlr <- ifelse(is.na(data.work$prixconcvlr), data.work$New_price , data.work$prixconcvlr) 

#print(subset(data.work, ident == 11339022, select = c("ident", "annee", "concvlr", "prixconcvlr", "New_price")) )


####  --- eqvltotal
#MyPlots(data.work, eqvltotal, laitproduit,  choix = 2)
#MyPlots(data.work, eqvltotal,  Profit, choix = 1)

#OK pas trop d'ouliers là!
## --->>>    Verifier qq prix à 0 pour prixconcvlr


### Recalculer le profit du coup!!! 

data.work <- data.work %>%
  mutate(Profit = prixlaiteriemoyen * laitproduit/1000
         - prixconcvlr * (concvlr * eqvltotal)/1000 
         - 15  * hasfpirri )


################ SAVING  ##############
# we save that data.work file

save(data.work, file=paste(Myroot,"Data/data.work.RData", sep=""))

# #We need to save the data in the directory for Shinyapps to work !!
file.copy(paste(Myroot,"Data/data.work.RData", sep=""),paste(Myroot,"Progs/Shiny/DataObserver", sep=""), overwrite = TRUE )

#################  UPLOADING  ############

# HERE is the working file ! 
load(paste(Myroot,"Progs/Shiny/dataobserver/data.work.RData", sep=""))


library(MASS)

parcoord(data.work[, c( "laitproduit","eqvltotal","sau","hasfpirri", "concvlr", "Tyear" )],  
         lwd=1, col = 1 + (0:1409)%/%50 , var.label=TRUE)


parallelplot(~data.work[, c(  "region4", "Tyear", "eqvltotal","sau", "hasfpirri", "concvlr", "laitproduit")], 
             groups = data.work$region4,
             horizontal.axis = FALSE,  scales = list(x = list(rot = 90)))

library(GGally)
ggparcoord(data = subset(data.work, hasfpirri !=0), columns = c(5,7,6,8,9), 
           scale = "uniminmax",
           groupColumn = "region4",
           alphaLines = 0.05 )

library(ggparallel)
ggparallel(list("laitproduit", "sau"), data=subset(data.work, hasfpirri !=0), method='parset', 
           alpha = 0.3)



library(DataExplorer)
CorrelationContinuous(data.work, use = "na.or.complete")

GenerateReport(data.work,
               output_file = paste("Report_data.work-", format(Sys.time(), "%Y-%m-%d"),".html", sep=""),
               output_dir = paste(Myroot,"Data", sep=""),
               html_document(toc = TRUE, toc_depth = 6, theme = "flatly"))



