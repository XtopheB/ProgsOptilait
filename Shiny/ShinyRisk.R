##
##  Exemples de graphiques dynamiques pour " Voir ou ne pas Voir" 
##   15 janvier 2015, Cafés de l'IMT
##   18 March 2015 GT Food-TSE
## 30/03/2016 : update with parallelrisk application and deployment on Shinyapps.io !
## 24/04/2017 : Change directory 

rm(list=ls())
#install.packages('shiny')
library(shiny)
library(np)

library(ggplot2)
library(ggthemes)
library(doBy)
library(quantreg)

## My examples :

Myroot <- "D:/progs/Optilait/Progs/"
#Myroot  <- "C:/Chris/progs/Optilait/"   

setwd(paste(Myroot,"Shiny/ParallelRisk/", sep=""))

EstDate <- "2015-05-23"

load(paste("Risk.all",EstDate,".RData", sep=""))




#Shiny version in subdirectory ShinyParallel

setwd(paste(Myroot,"Shiny/", sep=""))

#attach(risk.all)
runApp("ParallelRisk")


### ===============  Deploy ======================= ##
### Deploying on the web  !!! ### 

# Deploying demo on ShinyApps.io
# My account  https://www.shinyapps.io/admin/#/applications/running 


library(shinyapps)
shinyapps::deployApp("ParallelRisk")

# Now we can connect to https://xtophedataviz.shinyapps.io/ParallelRisk/


R <- 0 # choice of region; 0= All)
# Point Plot 
Plot.Point <- ggplot(risk.all, aes(x=years, y= AR)) + 
  geom_point(color = "grey", alpha=0.80) + 
  geom_point(dat= subset(risk.all, region==R), alpha=0.80, color="pink") +
  coord_cartesian(ylim = c(-40,40)) + guides(colour=FALSE)+ theme_classic()

Plot.Point

# Point Plot  with Jitter 
Plot.Jitter <- ggplot(risk.all, aes(x=years, y= AR)) + 
  geom_jitter(color = "grey",  alpha=0.80) + 
  geom_jitter(dat= subset(risk.all, region==R), alpha=0.50, color="pink") +
  coord_cartesian(ylim = c(-40,40)) + guides(colour=FALSE) +
  ggtitle(paste("Jittered points (region",R, "higlighted)")) +
  theme_classic()

Plot.Jitter


#LinePlot
risk.sum <- summaryBy(AR+theta+theta1+theta2+sigma+Profit+AR.N+theta.N+theta1.N+theta2.N+RP+RP.pc~years, data = risk.all, 
                      FUN = function(x) { c(med = median(x, na.rm=TRUE), 
                                            mean = mean(x),
                                            sd= quantile(x,probs= c(0.05,0.95), names= FALSE, na.rm=TRUE)) } )

Plot.Line <- ggplot(risk.sum, aes(years, AR.med, group=1)) +
  geom_point(color ="black") +
  geom_line(color= "red") +
  coord_cartesian(ylim = c(-40,40)) +
  theme_classic()

Plot.Line

#LinePlot + quantile

Plot.quantile <- Plot.Line + geom_pointrange(data = risk.sum, aes_string(ymin=AR.sd1, ymax = AR.sd2), 
                                             color = "grey", size=1)
Plot.quantile 

#Boxplot 
Plot.Box <- ggplot(risk.all, aes(x=years, y= AR)) + geom_boxplot(outlier.colour= "grey", aes(fill = years)) + 
  coord_cartesian(ylim = c(-40,40)) + guides(colour=FALSE, fill=FALSE)+ theme_classic()

Plot.Box

Plot.Box <- ggplot(data = risk.all, aes(x=years, y= AR)) + 
  geom_boxplot(outlier.colour= "grey", color= "darkgrey", fill="grey") + 
  geom_boxplot(data = subset(risk.all, region == R), outlier.colour= "pink", color= "darkgrey", fill="pink") + 
  coord_cartesian(ylim = c(-40,40)) + guides(colour=FALSE, fill=FALSE)+ theme_classic()

Plot.Box

# Parallel Spaghetti plot 
# defining the plot environnent (no geom_XXX here !) 
Plot.AR <- ggplot(risk.all, aes(x=years, y=AR, group=factor(ident) ))  + guides(colour=FALSE) + coord_cartesian(ylim = c(-40,40)) 
Plot.AR

Plot.Brush <- Plot.AR + geom_line(alpha=0.05, color="black") + theme_tufte()
Plot.Brush

#Simple plot on a subset 
Plot.ARP <- ggplot(subset(risk.all, region==4), aes(x=years, y=AR, group=factor(ident) ))  + guides(colour=FALSE) + coord_cartesian(ylim = c(-40,40)) 

Plot.BrushP= Plot.ARP + geom_line(alpha=0.05, color="red") + theme_classic()
Plot.BrushP


#Plot with two graphs in one (simple syntax) 

Plot.Tot <-  ggplot()  +
  geom_line(dat= risk.all, alpha=0.05, color="black", aes(x=years, y=AR, group=factor(ident) )) +
  geom_line(dat= subset(risk.all, region==R), alpha=0.05, color="pink",  aes(x=years, y=AR, group=factor(ident) )) +
  theme_classic() + guides(colour=FALSE) + coord_cartesian(ylim = c(-40,40)) 

Plot.Tot

# Small
risk.select <- subset(risk.all, region !=1 & region!=5 )

Plot.Tot + facet_wrap(~region)

risk.sum <- summaryBy(AR+theta+theta1+theta2+SigmaProf+Profit+AR+RP+RP.pc~year, data = risk.all, 
                      FUN = function(x) { c(miss = sum(is.na(x)), 
                                            tx = round(sum(is.na(x))/length(x), digits=3)) } )

#Plotting missing values rate  over time
Plot.miss <- ggplot(risk.sum, aes(year, AR.tx, group=1)) +
  geom_point(color ="black") +
  geom_line(color= "grey") +
  ggtitle("Missing Values rate") +
  theme_classic()  risk.sum <- summaryBy(AR+theta+theta1+theta2+SigmaProf+Profit+AR+RP+RP.pc~year, data = risk.all, 
                                         FUN = function(x) { c(miss = sum(is.na(x)), 
                                                               tx = round(sum(is.na(x))/length(x), digits=3)) } )



#Shiny version in subdirectory ShinyParallel

library(shiny)
setwd(paste(Myroot,"Presentations/Demos/", sep=""))


runApp("ShinyParallel")


### ===============  Deploy ======================= ##
### Deploying on the web  !!! ### 

# Deploying demo on ShinyApps.io
# My account  https://www.shinyapps.io/admin/#/applications/running 


#library(shinyapps)
#shinyapps::deployApp("ShinyParallel")

# Now we can connect to https://xtophedataviz.shinyapps.io/ShinyParallel/




# To Terminate == Archiving applications !!
shinyapps::terminateApp("ShinyParallel")











save(bw.f, duree.bw,  file = paste(Myroot,"Results/bw.f.",bwmethod,".",year,".RData", sep=""))


# Example with points
df <- data.frame(x = rnorm(5000), y = rnorm(5000))
h  <- ggplot(df, aes(x,y))
h + geom_point()

h + geom_point(alpha = 0.5)

h + geom_point(alpha = 1/10, colour="red")




