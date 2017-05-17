################## SninyParallel : SERVER   ################

library(shiny)
library(ggplot2)
library(ggthemes)
library(doBy)
library(dplyr)

# 
# shinyServer(func=function(input, output) {
#   load(paste("Risk.all",input$Date,".RData", sep=""))
#   # There may be some  variables to rename 
#   try(risk.all$year <- risk.all$years)
#   try(risk.all$region <- risk.all$region.x)
#   
#   attach(risk.all)
#   #load("Risk.all.RData")
#   Tx.Complete <- sum(complete.cases(risk.all)/nrow(risk.all))*100
# })
#   


# Define server logic for random distribution application
shinyServer(function(input, output, session) {

  # Command to activate interaction between ui and the file to load: here risk.data() 

  risk.data <- reactive({
        load(paste("Risk.all",input$Date,".RData", sep=""))
        risk.all
  })
  
  
  observe({
    
    updateSelectInput(session, "Y",
                      choices = colnames(risk.data())
    )
  
  
  output$summary <- renderPrint({
    summary(risk.data())
  })
  
  # Show the first "n" observations
  output$view <- renderTable({
    head(risk.data(), n = 10)
  })
  
  # output$MyTable <- renderDataTable({
  #   load(paste("Risk.all",input$Date,".RData", sep=""))
  #   # There may be some  variables to rename 
  #   #try(risk.all$year <- risk.all$years)
  #   #try(risk.all$region <- risk.all$region.x)
  #   
  #   print(head(risk.all))
  # })
  
  output$PointPlot <- renderPlot({
    R <- input$R
    A <- input$A
    
    #load(paste("Risk.all",input$Date,".RData", sep=""))
    #risk.all <- risk.data()
    
    # There may be some  variables to rename 
    #try(risk.all$year <- risk.all$years)
    #try(risk.all$region <- risk.all$region.x)
    
    
    # Many thanks to Thibault for those lines  
    minval <- -input$range[1]*min(risk.data()[,names(risk.data())%in%as.character(input$Y)], na.rm=T)/100
    maxval <- input$range[2]*max(risk.data()[,names(risk.data())%in%as.character(input$Y)], na.rm=T)/100
    
    
    # Point Plot 
    Plot.Point <- ggplot(risk.data(), aes_string(x="year", y=input$Y)) + 
      geom_point(color = "grey", alpha=A) + 
      geom_point(dat= subset(risk.data(), region==R), alpha=0.50, color="pink") +
      coord_cartesian(ylim = c(minval,maxval)) + guides(colour=FALSE)+
      ggtitle(paste("Overplotted points (Region",R, "higlighted)")) +
      theme_classic()
    
    Plot.Point

  })
  
  output$JitterPlot <- renderPlot({
  R <- input$R
  A <- input$A
  
  # Many thanks to Thibault for those lines  
  minval <- -input$range[1]*min(risk.data()[,names(risk.data())%in%as.character(input$Y)], na.rm=T)/100
  maxval <- input$range[2]*max(risk.data()[,names(risk.data())%in%as.character(input$Y)], na.rm=T)/100
  
  # Point Plot  with Jitter 
  Plot.Jitter <- ggplot(risk.data(), aes_string(x="year", y=input$Y)) + 
    geom_jitter(color = "grey",  alpha=A) + 
    geom_jitter(dat= subset(risk.data(), region==R), alpha=0.50, color="pink") +
    coord_cartesian(ylim = c(minval,maxval)) + 
    guides(colour=FALSE) +
    ggtitle(paste("Jittered points (Region",R, "higlighted)")) +
    theme_classic()
  
  Plot.Jitter
  })
  
  output$LinePlot <- renderPlot({
    
    # Many thanks to Thibault for those lines  
    minval <- -input$range[1]*min(risk.data()[,names(risk.data())%in%as.character(input$Y)], na.rm=T)/100
    maxval <- input$range[2]*max(risk.data()[,names(risk.data())%in%as.character(input$Y)], na.rm=T)/100
    
    risk.sum <- summaryBy(sau+sfp+quotalait+eqvltotal+laitproduit+concvlr+prixconcvlr+
                            prixlaiteriemoyen+charpot+hasfpirri+Tyear+ETPyear+DELTAyear+
                          AR+theta+theta1+theta2+Profit+AR~year, data = risk.data(), 
                          FUN = function(x) { c(med = median(x, na.rm=TRUE), mean = mean(x)) } )
    
    
    # We need an intermediate variable name
    Mavar <- paste(input$Y,".med", sep="") 
    
      Plot.Line <- ggplot(risk.sum, aes_string(x="year", y=Mavar, group=1)) +
      geom_point(color ="black", size=1) +
      geom_line(color= "grey", size=1) +
      coord_cartesian(ylim = c(minval,maxval)) +
        ggtitle(paste("Median Values, (  x% of complete cases)")) +
      # ggtitle(paste("Median Values, (", round(Tx.Complete, digits = 2)," % of complete cases)")) +
      theme_classic() 
    Plot.Line
    
  })
  
  output$QuantilePlot <- renderPlot({
    
    # Many thanks to Thibault for those lines  
    minval <- -input$range[1]*min(risk.data()[,names(risk.data())%in%as.character(input$Y)], na.rm=T)/100
    maxval <- input$range[2]*max(risk.data()[,names(risk.data())%in%as.character(input$Y)], na.rm=T)/100
    
    #Function  that computes median and mean values !
    risk.sum <- summaryBy(sau+sfp+quotalait+eqvltotal+laitproduit+concvlr+prixconcvlr+
                            prixlaiteriemoyen+charpot+hasfpirri+Tyear+ETPyear+DELTAyear+
                            AR+theta+theta1+theta2+Profit+AR~year, data = risk.data(),  
                          FUN = function(x) { c(med = median(x, na.rm=TRUE), 
                                                mean = mean(x),
                                                sd= quantile(x,probs= c(0.05,0.95), names= FALSE, na.rm=TRUE)) } )
    # We need an intermediate variable name
    Mavar <- paste(input$Y,".med", sep="") 
    
    Plot.Line <- ggplot(risk.sum, aes_string(x="year", y=Mavar, group=1)) +
      geom_point(color ="black") +
      geom_line(color= "grey") +
      coord_cartesian(ylim = c(minval,maxval)) +
      ggtitle("Median Values") +
      theme_classic() 
    
    # We need intermediate variables names
   Mavar1 <- paste(input$Y,".sd1", sep="") 
   Mavar2<- paste(input$Y,".sd2", sep="") 
   
   Plot.quantile <- Plot.Line + geom_pointrange(data = risk.sum, aes_string(ymin=Mavar1, ymax = Mavar2), 
                                               color = "grey", size=1) +
    ggtitle("Median Values + quantiles") +
    theme_classic() 
  
  Plot.quantile 
  })
  
  output$BoxPlot <- renderPlot({
    R <- input$R
    
    # Many thanks to Thibault for those lines  
    minval <- -input$range[1]*min(risk.data()[,names(risk.data())%in%as.character(input$Y)], na.rm=T)/100
    maxval <- input$range[2]*max(risk.data()[,names(risk.data())%in%as.character(input$Y)], na.rm=T)/100
    
    #BoxPlot
    Plot.Box <- ggplot(data = risk.data(), aes_string(x="year", y=input$Y)) +  
      geom_boxplot(outlier.colour= "grey", color= "darkgrey", fill="grey") + 
      geom_boxplot(data = subset(risk.data(), region == R), outlier.colour= "pink", color= "darkgrey", fill="pink") + 
      coord_cartesian(ylim = c(minval,maxval)) + 
      guides(colour=FALSE, fill=FALSE)+
      ggtitle(paste("Boxplots")) +
      theme_classic() 
    
    Plot.Box
    
  })
  
  output$ParaPlot <- renderPlot({
    R <- input$R
    A <- input$A
    
    # Many thanks to Thibault for those lines  
    minval <- -input$range[1]*min(risk.data()[,names(risk.data())%in%as.character(input$Y)], na.rm=T)/100
    maxval <- input$range[2]*max(risk.data()[,names(risk.data())%in%as.character(input$Y)], na.rm=T)/100
    
    #Parallel plot 
    Plot.Tot <-  ggplot()  +
      geom_line(dat= risk.data(), alpha=A, color="black", 
                aes_string(x="year", y=input$Y, group="factor(ident)" )) +
      geom_line(dat= subset(risk.data(), region==R), alpha=0.05, color="pink",
                aes_string(x="year", y=input$Y, group="factor(ident)" )) +
      guides(colour=FALSE) + 
      coord_cartesian(ylim = c(minval,maxval)) +
      ggtitle(paste("Parallel Spaghetti Plot (Region",R, "higlighted)")) +
      theme_classic() 
    
    Plot.Tot
    
  })
  output$ParaMulti <- renderPlot({
    A <- input$A

    # Many thanks to Thibault for those lines  
    minval <- -input$range[1]*min(risk.data()[,names(risk.data())%in%as.character(input$Y)], na.rm=T)/100
    maxval <- input$range[2]*max(risk.data()[,names(risk.data())%in%as.character(input$Y)], na.rm=T)/100
    
    #Multiple parallel plot
    Plot.Multi <-  ggplot()  +
      geom_line(dat= risk.data(), alpha=A, color="black", aes_string(x="year", y=input$Y, group="factor(ident)" )) +
      guides(colour=FALSE) + 
      coord_cartesian(ylim = c(minval,maxval))
    
  Plot.Multi + facet_wrap(~region)  +
      ggtitle(paste("Multiple Parallel Spaghetti Plot")) +
      theme_classic() 
  })
  
  #Missing values 
  output$Missing <- renderPlot({
   
    minval <- input$range[1]
    maxval <- input$range[2]
    risk.sum <- summaryBy(AR+theta+theta1+theta2+SigmaProf+Profit+AR+RP+RP.pc~year, data = risk.data(), 
                          FUN = function(x) { c(miss = sum(is.na(x)), 
                                                tx = round(sum(is.na(x))/length(x), digits=3)) } )
    
    # We need an intermediate variable name
    Mamissvar <- paste(input$Y,".tx", sep="") 
    
    Plot.miss <- ggplot(risk.sum, aes_string(x="year", y=Mamissvar, group=1)) +
      geom_point(color ="black") +
      geom_line(color= "grey") +
      coord_cartesian(ylim = c(minval,maxval)) +
      ggtitle("Missing Values rate (in % of the sample)") +
      theme_classic() 
    

    
    Plot.miss 
  })
  })
  
})
  
