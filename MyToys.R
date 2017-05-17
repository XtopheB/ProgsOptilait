# My functions (graphics, etc...)
# 15/05/ 2017 


library(dplyr)
library(lattice)
library(ggplot2)
library(Hmisc)


MyPlots <- function(mydata, varx, vary, choix = 1){
  mydata$y <- eval(substitute(vary), mydata)  
  mydata$x <- eval(substitute(varx), mydata)  
  
  if(choix == 1) {
    xyplot(y ~ x | region4, mydata,
           index.cond=list(c(1,2,3,4)), # this provides the order of the panels
           group = region ,
           grid = TRUE,
           xlab= paste("X=", deparse(substitute(varx)), ""),
           ylab= paste("Y=", deparse(substitute(vary)), ""),
           #       panel = panel.smoothScatter,
           type = c("p", "r"), alpha = 0.7, lwd = 1,
           par.settings = list(strip.background=list(col="lightgrey")))
    
  }
  else {
    pg <- ggplot(mydata, aes(x, y )) + 
      geom_point(col = "grey", alpha = 0.5) + 
      facet_wrap(~region4, nrow = 2) + 
      geom_smooth(method = "lm", se = F, fullrange = T, colour = "steelblue", size = 0.2) +
      xlab(paste("X=", deparse(substitute(varx)), "")) +
      ylab(paste("Y=", deparse(substitute(vary)), "")) +
      theme_classic()
    
    pg
  }
  
}


MyPlotsCond <- function(mydata, varx, vary, varcond = region4, choix = 1){
  mydata$y <- eval(substitute(vary), mydata)  
  mydata$x <- eval(substitute(varx), mydata) 
  mydata$cond <- eval(substitute(varcond), mydata) 
  
  if(choix == 1) {
    xyplot(y ~ x | cond , mydata,
           index.cond= list(c(1,2,3,4)), # this provides the order of the panels
           group = region ,
           grid = TRUE,
           xlab= paste("X=", deparse(substitute(varx)), ""),
           ylab= paste("Y=", deparse(substitute(vary)), ""),
           #       panel = panel.smoothScatter,
           type = c("p", "r"), alpha = 0.7, lwd = 1,
           par.settings = list(strip.background=list(col="lightgrey")))
    
  }
  else {
    pg <- ggplot(mydata, aes(x, y )) + 
      geom_point(col = "grey", alpha = 0.5) + 
      facet_wrap(~cond , nrow = 2) + 
      geom_smooth(method = "lm", se = F, fullrange = T, colour = "steelblue", size = 0.2) +
      xlab(paste("X=", deparse(substitute(varx)), "")) +
      ylab(paste("Y=", deparse(substitute(vary)), "")) +
      theme_classic()
    
    pg
  }
  
}


MyCook <- function(mydata, varx, vary){
  mydata$y <- eval(substitute(vary), mydata)  
  mydata$x <- eval(substitute(varx), mydata)  
  
  # Computing Cooks distance
  mod <- lm(y ~ x + region4 , data = mydata)
  cooksd <- cooks.distance(mod)
  
  #indice des gros
  ii <- which(cooksd>4*mean(cooksd, na.rm=T))
  
  # plot cook's distance
  plot(cooksd, pch="o", col = "grey", main="Influential Obs by Cooks distance")  
  abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
  points(x=1:length(cooksd)+1, y=cooksd, col="red")  # add labels
  
  #Test
  print(car::outlierTest(mod))
  
  View(subset(mydata[ii[1:10] ,c(1,2, 5:10)]))
  
}

# MyCook(data.work, laitproduit, eqvltotal)

# see https://www.stat.ubc.ca/~jenny/STAT545A/block09_xyplotLattice.html