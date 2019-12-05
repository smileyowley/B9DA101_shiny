rm(list = ls())
if(!require("shiny")) {install.packages("shiny")}
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("ggcorrplot")) install.packages("ggcorrplot")
if(!require("RODBC")) {install.packages("RODBC")}
if(!require("GGally")) {install.packages("GGally")}
if(!require("gridExtra")) {install.packages("gridExtra")}
if(!require("magrittr")) {install.packages("magrittr")}
##if(!require("ggpubr")) {install.packages("ggpubr")}
if(!require("ggthemes")) {install.packages("ggthemes")}
if(!require("dplyr")) {install.packages("dplyr")}
if(!require("plot3D")) {install.packages("plot3D")}
library(ggplot2)
library(ggcorrplot)
library(shiny)
library(RODBC)
##
library(GGally)
library(gridExtra)
library(magrittr)
###library(ggpubr)
library(ggthemes)
library(dplyr)
library(plot3D)


create_box_plot <- function(pDataframe, pCategorical, pNumerical, pTitle, pCategoricalDescription, pNumbericDescription)
{
  # Boxplot of scores and Test Prep by Gender
  b <- ggplot(pDataframe, aes(pCategorical, pNumerical))
  b <- b + geom_boxplot()
  b <- b + ggtitle(pTitle)
  b <- b + xlab(pCategoricalDescription) + ylab(pNumbericDescription)
  b
}


create_count_histogram_With_Gradient_filling <- function(pDataframe, pCategoricalVar, pMainTitle, pXtitle, pYtitle)
{
  ### Bar for categorical variables
  ggplot(data=pDataframe, aes(x = pCategoricalVar)) +
    geom_bar(
      col="red",
      aes(fill = ..count..),                                            ###  Shades bars according to count
      alpha = .5                                                      ###  transparency of bars 0 (fully transparent) -> 1 (fully opaque)
    ) +
    scale_fill_gradient("Count", low="green", high = "red") +         ###  alternative fill setting colours according to count
    labs(title = pMainTitle, x = pXtitle, y = pYtitle)
}

create_pie_chart <- function(pDataframe, pCategoricalVar, pMainTitle, pCaption, pLabel)
{
  ### Build a pie chart
  pie <- ggplot(pDataframe, aes(x = "", fill = pCategoricalVar)) + 
    geom_bar(width = 1) +
    theme(axis.line = element_blank(), 
          plot.title = element_text(hjust=0.5)) + 
    labs(fill = pLabel, 
         x=NULL, 
         y=NULL, 
         title=pMainTitle, 
         caption=pCaption)
  pie + coord_polar(theta = "y", start=0)
}

create_scatterplot_for_each_categorical_variable <- function(pDataframe, pXvar, pyVar, pCategoricalVar, pMainTitle, pXTitle, pYtitle, pCaption)
{
  ### Stacked Scatterplot
  ggplot(pDataframe, aes(x = pXvar, y = pyVar, col = pyVar)) +
    geom_point() +
    facet_grid(rows = pCategoricalVar, scales = "free") +                                ## With facet_grid individual graph by Diet otherwise not broken down
    labs(title = pMainTitle, x = pXTitle, y = pYtitle, caption = pCaption) +
    stat_smooth(method = "lm", col = "red", se = FALSE)
}

create_correlogram  <- function(pContinuousVar, pTitle)
{
  corr <- round(cor(pContinuousVar),1)
  ggcorrplot(corr, hc.order = TRUE,
             type = "full",
             lab = TRUE,
             lab_size = 4,
             method = "circle",
             colors = c("tomato2", "white", "springgreen3"), 
             title=pTitle, 
             ggtheme=theme_classic())
}

ui <- fluidPage(
    titlePanel("File Load"),
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Slider for the number of bins ----
        
        tags$hr(),
        ###
        ###
        ###   File load
        ###
        
        selectizeInput(inputId = "dataMethod", label = "Select Method of entering data", choices = c("csv File", "URL"), selected = NULL, multiple = TRUE, options = list(maxItems = 1, minItems = 1)),
        conditionalPanel(
          condition = "input.dataMethod == 'csv File'",
          checkboxInput('header', 'Header', TRUE),
          radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma = ',',  Semicolon = ';', Tab = '\t')),
          radioButtons('quote', 'Quote', c(None = '', 'Double Quote' = '"', 'Single Quote' = "'")),
          fileInput('file', 'Choose CSV File', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
        ),
        conditionalPanel(
          condition = "input.dataMethod == 'URL'",
          textInput(inputId = "URL", label = "Input URL", value = "http://www.stat.ufl.edu/~winner/data/nfl2008_fga.csv")
        ),
        conditionalPanel(
          condition = "input.dataMethod == 'csv File' || input.dataMethod == 'URL'",
          actionButton(inputId = "btnLoadData", label = "Load Data")
        ),
        conditionalPanel(condition = "output.fileUploaded",
            ### radioButtons(inputId = "plottype", label = "Select Plot Type", choices = c("Scatterplot", "Correlogram", "Histogram", "Pie", "Hypothesis"), selected = NULL)
            radioButtons(inputId = "plottype", label = "", choices = c(""), selected = NULL)
        ),
        conditionalPanel(
          condition = "input.plottype == 'Pie' || input.plottype == 'Scatterplot' || input.plottype == 'Histogram' || input.plottype == 'Boxplot'",
          selectizeInput(inputId = "genAll", label = "Select Categorical", choices = "")
        ),
        conditionalPanel(
          condition = "input.plottype == 'Scatterplot' || input.plottype == 'Correlogram' || input.plottype == 'Hypothesis' || input.plottype == 'Boxplot'",
          selectizeInput(inputId = "genNumber", label = "Select Numerical", choices = "")
        ),
        conditionalPanel(
          condition = "input.plottype == 'Hypothesis'",
          numericInput("mu", "Enter mu value:", 5, min = 1, max = 100),
          radioButtons("alpha", "Significance Level :",
                       c("0.01" = 0.01,
                         "0.05" = 0.05,
                         "0.1" = 0.1)),
          selectInput("tail", "Tail :",
                      c("Lower (-ve)" = "less",
                        "Upper (+ve)"= "greater",
                        "Two" = "two.sided"))
        ),
        conditionalPanel(
          condition = "input.plottype == 'Scatterplot'",
          actionButton(inputId = "runScatter", label = "Run Scatterplot")
        ),
        conditionalPanel(
          condition = "input.plottype == 'Histogram'",
          actionButton(inputId = "runHistogram", label = "Run Histogram")
        ),
        conditionalPanel(
          condition = "input.plottype == 'Correlogram'",
          actionButton(inputId = "runCorrelogram", label = "Run Corrolegram")
        ),
        conditionalPanel(
          condition = "input.plottype == 'Pie'",
          actionButton(inputId = "runPie", label = "Run Pie Chart")
        ),
        conditionalPanel(
          condition = "input.plottype == 'Hypothesis'",
          actionButton(inputId = "runHypothesis", label = "Run Hypothesis Test")
        ),
        conditionalPanel(
          condition = "input.plottype == 'Boxplot'",
          actionButton(inputId = "runBoxplot", label = "Run Boxplot")
        ),
        ### Selection Criteria for ScatterPlot
        conditionalPanel(condition = "output.fileUploaded",
          selectizeInput(inputId = "tableFieldSelection", label = "Select Fields for Table Display", choices = "")
        )
      ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel('Table', tableOutput('contents')),
                  tabPanel('GenPlot', plotOutput('generic')),
                  tabPanel('Diagnostics', tableOutput('diagnostics')),
                  tabPanel('Hypothesis Testing', verbatimTextOutput('prob'))
    )
  )
))


server <- shinyServer(function(input, output, session) {

  
  ###  Load data
  dataread <- eventReactive(input$btnLoadData, {
    print("dataread <- eventReactive(input$btnLoadData")
    if (input$dataMethod == "csv File")
      {
        if ( is.null(input$file)) return(NULL)
        inFile <- input$file
        file <- inFile$datapath
        print(paste("File = ", file))
        read.csv(file, header = input$header, sep = input$sep, quote = input$quote)
      }
    else
      {
        read.csv(input$URL)
      }
  } )
  
  pCaption <- eventReactive(input$btnLoadData, {
    if (input$dataMethod == "csv File") input$file$name
    else input$SQL
  } )
  
  ### Once data is loaded into dataread() load filterable data into mydata()
  mydata <- eventReactive(input$tableFieldSelection, {
    print("mydata <- eventReactive(input$tableFieldSelection")
    if (is.null(dataread()))
    {
      print("NULL mydata")
      return(NULL)
    }
    print("mydata <- eventReactive(dataread()")
    dataread()
  })
  
  ### Determine class of all fields for plotting etc.
  dataread_str <- eventReactive(dataread(), {
      print("dataread_str <- eventReactive(dataread()")
      print(input$file$name)
      cbind(colnames(dataread()),(sapply(dataread(), class)))
    }
  )
  
  ### Update all input fields for selection in table rendering
  observeEvent( dataread_str(), {
      print("observeEvent( dataread_str()")  
      updateRadioButtons(session = session, inputId = "plottype", choices = c("Scatterplot", "Correlogram", "Histogram", "Pie", "Hypothesis", "Boxplot"), selected = NULL)
      updateSelectizeInput(session = session, inputId = "tableFieldSelection",  choices = colnames(dataread()), selected = colnames(dataread()), options = list(maxItems = 999, minItems = 1))
      updateSelectizeInput(session = session, inputId = "genAll", label = "Category", choices = "")
      updateSelectizeInput(session = session, inputId = "genNumber", label = "Numbers", choices = "")
  } )
  
  ###  After selecting plot type setup selection criteria
  observeEvent(input$plottype, {
    print(paste("Plot selected : ", input$plottype))
    if (input$plottype == "Scatterplot")
    {
      updateSelectizeInput(session = session, inputId = "genAll", label = "Scatterplot Category", choices = (dataread_str()[dataread_str()[,2] == "factor", 1]), options = list(maxItems = 1, minItems = 1))
      updateSelectizeInput(session = session, inputId = "genNumber", label = "Scatterplot XVar & YVar", choices = (dataread_str()[dataread_str()[,2] != "factor", 1]), options = list(maxItems = 2, minItems = 2))
    }
    else if (input$plottype == "Histogram")
    {
      updateSelectizeInput(session = session, inputId = "genAll",label = "Histogram Variable", choices = (dataread_str()[,1]), options = list(maxItems = 1, minItems = 1))
    }
    else if (input$plottype == "Correlogram")
    {
      updateSelectizeInput(session = session, inputId = "genNumber", label = "Correlogram Variables", choices = (dataread_str()[dataread_str()[,2] != "factor", 1]), options = list(maxItems = 9999, minItems = 1))
    }
    else if (input$plottype == "Pie")
    {
      updateSelectizeInput(session = session, inputId = "genAll", label = "Pie Category", choices = (dataread_str()[dataread_str()[,2] == "factor", 1]), options = list(maxItems = 1, minItems = 1))
    }
    else if (input$plottype == "Hypothesis")
    {
      updateSelectizeInput(session = session, inputId = "genNumber", label = "Select a column", choices = (dataread_str()[dataread_str()[,2] != "factor", 1]), options = list(maxItems = 1, minItems = 1))
    }
    else if (input$plottype == "Boxplot")
    {
      updateSelectizeInput(session = session, inputId = "genAll", label = "Boxplot Category", choices = (dataread_str()[dataread_str()[,2] == "factor", 1]), options = list(maxItems = 1, minItems = 1))
      updateSelectizeInput(session = session, inputId = "genNumber", label = "Boxplot Variable", choices = (dataread_str()[dataread_str()[,2] != "factor", 1]), options = list(maxItems = 1, minItems = 1))
    }
  } )
  
  ### Gather data for each plot event
  scatterEvent <- eventReactive(input$runScatter, {
    cbind(dataread()[,input$genAll],dataread()[,input$genNumber])
  })
  
  boxplotEvent <- eventReactive(input$runBoxplot, {
    dataread()
  })
  
  histogramEvent <- eventReactive(input$runHistogram, {
    dataread()
  })
  
  correlogramEvent <- eventReactive(input$runCorrelogram, {
    na.omit(dataread()[,input$genNumber])
  })
  
  pieEvent <- eventReactive(input$runPie, {
    dataread()
  })
  
  hypothesisEvent <- eventReactive(input$runHypothesis, {
    x <- dataread()[,input$genNumber]
    conf_level = (1-as.numeric(input$alpha)) 
    t=t.test(x,mu= input$mu, alternative= input$tail,conf.level=conf_level)  
    
    alpha <- as.numeric(input$alpha)
    p_value <- as.numeric(t$p.value)
    
    if(p_value < alpha){ 
      decision='Reject H_0'}
    else{ 
      decision='Accept H_0' 
    } 
    
    (paste(conf_level, ' The decision is to ', decision))
    
    L <- mean(x)-abs(qnorm(alpha/2))*sd(x)/sqrt(length(x)) 
    U <- mean(x)+abs(qnorm(alpha/2))*sd(x)/sqrt(length(x)) 
    conf_Inter <-  c(L, U) 
    ### (paste(conf_level, ' The decision is to ', decision, "<br>", 'Confidence Interval Lower :', L,'Upper :', U)) 
    eOutput <- as.list(c(paste(conf_level, ' The decision is to ', decision), paste('Confidence Interval Lower :', L,'Upper :', U)))
    eOutput
  })

  output$prob <- renderPrint(
    hypothesisEvent()
  )
  output$diagnostics <- renderTable(
    dataread_str()
  )
  
  output$fileUploaded <- reactive({
    return(!is.null(dataread()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  output$contents <- renderTable(
    if (length(input$tableFieldSelection) == 0)
      return(NULL)
    else
      mydata()[,input$tableFieldSelection]
  )
  
  output$generic <- renderPlot(
    
    ### pDataframe, pXvar, pyVar, pCategoricalVar, pMainTitle, pXTitle, pYtitle, pCaption
    if (is.null(input$plottype))
    {
      NULL
    }
    else if (input$plottype == "Scatterplot")
    {
      create_scatterplot_for_each_categorical_variable(scatterEvent(), scatterEvent()[,2], scatterEvent()[,3], scatterEvent()[,1], paste('Scatterplot by', input$genAll), input$genNumber[1], input$genNumber[2], pCaption())
    }
    else if (input$plottype == "Correlogram")
    {
      create_correlogram(correlogramEvent(), "Correlogram of Scores")
    }
    else if (input$plottype == "Histogram")
    {
      create_count_histogram_With_Gradient_filling(histogramEvent(), histogramEvent()[,input$genAll], input$genAll, colnames(histogramEvent()[,input$genAll]), "Count")
    }
    else if (input$plottype == "Pie")
    {
      create_pie_chart(pieEvent(), pieEvent()[,input$genAll], input$genAll, pCaption(), input$genAll)
    }
    else if (input$plottype == "Boxplot")
    {
      create_box_plot(boxplotEvent(), boxplotEvent()[,input$genAll], boxplotEvent()[,input$genNumber], pCaption(), input$genAll, input$genNumber)
    }
    else NULL
  )
  
})

shinyApp(ui = ui, server = server)

