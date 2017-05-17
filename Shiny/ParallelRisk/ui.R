################## SninyParallel :UI   ################
library(shiny)
load("Risk.all.RData")
# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Simulation Results (22/03/2016)"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(   
    sliderInput("R", 
                "Select Region to highlight", 
                value = 0,  #  0 = no region highlighted !
                min = 0, 
                max = 6,
                step = 1),
    
    sliderInput("A", 
                "Select the transparancy level (Brushing)", 
                value = 0.1,  #  1= Plain black
                min = 0, 
                max = 1,
                step = 0.01), 
    
    selectInput("Y", "Variable", names(risk.all), selected = "AR"), 
    
    selectInput("Date", 
                "Date of Estimation",
                c("2015-05-23", "2015-12-05", "2016-03-17", "Historical" ),
                selected = "2016-03-17"), 
    
    # http://shiny.rstudio.com/articles/dynamic-ui.html
    
    # selectInput("dataset", "Dataset", c("2015-05-23", "2015-12-05", "2016-03-17" )),
    
    # conditionalPanel(
    #   condition = "output.data",
    #   checkboxInput("headonly", "Only use first 1000 rows")),
    # 
    
    sliderInput("range", 
                "Plot Min-Plot Max (in % of sample Min and Max) ", 
                -100, 100,
                step=2, value = c(-40, 40))
    
  ),
  
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Summ",verbatimTextOutput("summary")),
      tabPanel("Data",tableOutput("view")),
      tabPanel("Point Plot", plotOutput("PointPlot")),
      tabPanel("Line Plot", plotOutput("LinePlot")),
      tabPanel("Quantile Plot", plotOutput("QuantilePlot")),
      tabPanel("Box Plot", plotOutput("BoxPlot")),
      tabPanel("Jitter Plot", plotOutput("JitterPlot")),
      tabPanel("Spaghetti Plot", plotOutput("ParaPlot")),
      tabPanel("Multiple Spaghetti", plotOutput("ParaMulti")),
      tabPanel("Missing", plotOutput("Missing"))
      
    )
  )
))