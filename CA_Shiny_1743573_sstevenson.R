rm(list = ls())
if(!require("shiny")) {install.packages("shiny")}
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("ggcorrplot")) install.packages("ggcorrplot")
if(!require("RODBC")) {install.packages("RODBC")}
library(ggplot2)
library(ggcorrplot)
library(shiny)
library(RODBC)

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
        
        selectInput(inputId = "dataMethod", label = "Select Method of entering data", choices = c("csv File", "SQL"), selected = ""),
        conditionalPanel(
          condition = "input.dataMethod == 'csv File'",
          checkboxInput('header', 'Header', TRUE),
          radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma = ',',  Semicolon = ';', Tab = '\t')),
          radioButtons('quote', 'Quote', c(None = '', 'Double Quote' = '"', 'Single Quote' = "'")),
          fileInput('file', 'Choose CSV File', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))
        ),
        conditionalPanel(
          condition = "input.dataMethod == 'SQL'",
          textInput(inputId = "SQL", label = "Enter SQL")
        ),
        conditionalPanel(
          condition = "input.dataMethod == 'csv File' || input.dataMethod == 'SQL'",
          actionButton(inputId = "btnLoadData", label = "Load Data")
        ),
        conditionalPanel(condition = "output.fileUploaded",
            radioButtons(inputId = "plottype", label = "Select Plot Type", choices = c("Scatterplot", "Correlogram", "Histogram", "Pie"), selected = "")
        ),
        conditionalPanel(
          condition = "input.plottype == 'Pie' || input.plottype == 'Scatterplot' || input.plottype == 'Histogram'",
          selectizeInput(inputId = "genAll", label = "Select Categorical", choices = "")
        ),
        conditionalPanel(
          condition = "input.plottype == 'Scatterplot' || input.plottype == 'Correlogram'",
          selectizeInput(inputId = "genNumber", label = "Select Categorical", choices = "")
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
        ### Selection Criteria for ScatterPlot
        selectizeInput(inputId = "tableFieldSelection", label = "Select Fields for Table Display", choices = "")
      ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel('Table', tableOutput('contents')),
                  tabPanel('GenPlot', plotOutput('generic')),
                  tabPanel('Diagnostics', tableOutput('diagnostics'))
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
        myConn <- odbcConnect("TeraDW")
        x <- sqlQuery(myConn, input$SQL)
        odbcClose(myConn)
        x
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
      updateRadioButtons(session = session, inputId = "plottype", choices = c("Scatterplot", "Correlogram", "Histogram", "Pie"), selected = "")
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
  } )
  
  ### Gather data for each plot event
  scatterEvent <- eventReactive(input$runScatter, {
    cbind(dataread()[,input$genAll],dataread()[,input$genNumber])
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
    else NULL
  )
  
})

shinyApp(ui = ui, server = server)

