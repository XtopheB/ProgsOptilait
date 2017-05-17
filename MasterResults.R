#  MasterResults.R : Compiling the results
# 12/05/2017 : Recovering and compiling the results 


rm(list=ls())
#library(plyr)
library(dplyr)
library(ggplot2)
library(DataExplorer)

#Myroot <- "D:/progs/Optilait/"
Myroot <- "C:/Chris/progs/Optilait/"

#First we load the data refined by DataManager.R (and used in the Shiny app)
load(paste(Myroot,"Progs/Shiny/dataobserver/data.work.RData", sep=""))

#Then we load the result data computed by MasterNP.R (for all years)
yearlist<- c("2002", "2003", "2004", "2005")
samplereduc <- 1 

risk.all <- data.frame()
for (y in 1:length(yearlist)) {

  load(paste(Myroot,"Results/Risknp.CV.",yearlist[y],"-",samplereduc,".RData", sep=""))
 
  risk.foo <- risk.results 
  #risk.year  <- merge(data.all, risk.foo, by = c("ident","annee", "region"))
  risk.year  <- merge(data.work, risk.foo, by = c("ident","annee"))
  risk.all<- rbind.data.frame(risk.all, risk.year)
}


### Computing AR based on theta1 ##
risk.all <- risk.all %>% 
  mutate(
    AR1 = - theta1/SigmaProf
  )

## -----  First descriptive statistics ----

# Loading my ploting functions 
source(paste(Myroot,"Progs/MyToys.R", sep=""))


### Boxplot
Plot.Box <- ggplot(data = subset(risk.all,abs(AR) <10), aes(year, AR))+
  geom_boxplot(outlier.colour= "pink", color= "darkgrey", fill="pink") + 
  guides(colour=FALSE, fill=FALSE)+
  facet_wrap(~ region4) +
  ggtitle(paste("Boxplots")) +
  theme_classic() 
Plot.Box


#Spaghetti

ggplot(risk.all, aes(x = year, y = theta1, group = ident)) +
  geom_line(show.legend = FALSE,  alpha = 0.05) +
  facet_wrap(~ region4) +
#  coord_cartesian(ylim = c(-20.1,20.1)) +
  theme_minimal()



MyPlotsCond(subset(risk.all, abs(AR)<10), sau, AR, varcond = year, choix =2)

# ---- Results for one year: 

year.chosen <- 2003
data.year <- subset(data.work, year == year.chosen)
load(paste(Myroot,"Results/np.CV.",year.chosen,"-",samplereduc,".RData", sep=""))

data.year.results <-merge(data.year, risk.results, by = c("ident","annee"))

attach(data.year.results)

### Visualising derivatives

boxplot(f1.np)
boxplot(subset(data.year.results, abs(f1.np) <1000 & abs(f2.np) <1000 , select = c(f1.np, f2.np)), horizontal = TRUE)

# Revisiting npplots

n.calc<-c(100)

# Defining reference values for plotting f(x | z== value)

#grid for x variable 
concvlr.calc <- seq(quantile(concvlr,.1),quantile(concvlr,.9), length=n.calc)

# median for ALL

sau.calc <- uocquantile(sau,.5)
eqvltotal.calc <- uocquantile(eqvltotal,.5)
# concvlr.calc <- uocquantile(concvlr,.5)    # <-- x variable 
hasfpirri.calc<- uocquantile(hasfpirri,.5)  #  <--- better choice ???? 

Tyear.calc <- uocquantile(Tyear,.5)
ETPyear.calc <- uocquantile(ETPyear,.5)
DELTA.calc <- uocquantile(DELTAyear,.5)


# For discrete variable, we chose here !!
region4.calc<-c(2)


x.eval <- expand.grid( sau = sau.calc,
                       eqvltotal = eqvltotal.calc,
                       concvlr = concvlr.calc, 
                       hasfpirri = hasfpirri.calc,
                       region4 = region4.calc,
                       Tyear = Tyear.calc,
                       ETPyear = ETPyear.calc,
                       DELTA = DELTA.calc)

summary(x.eval)                      

data.eval <- data.frame(sau=x.eval[,1], eqvltotal=x.eval[,2], concvlr=x.eval[,3], hasfpirri=x.eval[,3],  
                        region4=factor(x.eval[,5]), Tyear=x.eval[,6], ETPyear=x.eval[,7], DELTAyear=x.eval[,8])

model.np <- npreg( bws = bw.f,
               gradients = TRUE,
               residuals = TRUE)
f.np.calc <- predict(model.np, xdat = data.eval)

plot(concvlr.calc , f.np.calc)

##  just to check something  : Is the chosen CV criterion oversmoothing the results ?
#  <---------------------------------- CHANGE HERE !!!!!!
bw2<-bw
bw2$xbw[1]<-0.5*bw$xbw[1]  # pxepp
bw2$xbw[2]<-0.5*bw$xbw[2]  # reves
cfhat <- fitted(npcdens(bws=bw2, newdata=data.eval))



## Graphiques sur REVEnueS ------
## define x-axis variable  according to grid !!

par(mfrow=c(2,1))
plot(reves.calc, cfhat, type="l", xlim=c(quantile(reves,.1),quantile(reves,.9)))
plot(reves.calc, cflogit, type="l", xlim=c(quantile(reves,.1),quantile(reves,.9)))

dev.off()
plot(reves.calc, cfhat, type="l", xlim=c(quantile(reves,.1),quantile(reves,.9)),ylim=c(min(cfhat,cflogit),max(cfhat,cflogit)))
lines(reves.calc, cflogit, col="red")
abline(.5,0)


summary(bw.f)

f.hat <- npreg(bw.f)

## plot(f.hat, plot.errors.method= "bootstrap")

npplot(bw.g)
