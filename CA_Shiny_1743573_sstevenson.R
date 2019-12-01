rm(list = ls())
if(!require("shiny")) {install.packages("shiny")}
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("ggcorrplot")) install.packages("ggcorrplot")
library(ggplot2)
library(ggcorrplot)
library(shiny)

create_count_histogram_With_Gradient_filling <- function(pDataframe, pCategoricalVar, pMainTitle, pXtitle, pYtitle)
{
  ### Bar for categorical variables
  ggplot(data=pDataframe, aes(x = pCategoricalVar)) +
    geom_bar(
      col="red",
      aes(fill=..count..),                                            ###  Shades bars according to count
      alpha = .5                                                      ###  transparency of bars 0 (fully transparent) -> 1 (fully opaque)
    ) +
    scale_fill_gradient("Count", low="green", high = "red") +         ###  alternative fill setting colours according to count
    labs(title = pMainTitle, x = pXtitle, y = pYtitle)
}

create_pie_chart <- function(pDataframe, pCategoricalVar, pMainTitle, pCaption)
{
  ### Build a pie chart
  pie <- ggplot(pDataframe, aes(x = "", fill = pCategoricalVar)) + 
    geom_bar(width = 1) +
    theme(axis.line = element_blank(), 
          plot.title = element_text(hjust=0.5)) + 
    labs(fill="weight", 
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
    titlePanel('input$file'),
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Slider for the number of bins ----
        
        tags$hr(),
        checkboxInput('header', 'Header', TRUE),
        radioButtons(inputId = 'sep', label = 'Separator',
                     choices = c(Comma=',',
                       Semicolon=';',
                       Tab='\t')),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'")),
        fileInput('file', 'Choose CSV File',
            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')
        ),
        conditionalPanel(condition = "output.fileUploaded",
                         radioButtons(inputId = "plottype", label = "Select Plot Type", choices = c("Scatterplot", "Correlogram", "Histogram"), selected = "")
        ),
        ### Selection Criteria for ScatterPlot
        conditionalPanel(
          condition = "input.plottype == 'Scatterplot'",
          ####selectInput(inputId = "scatterNumeric", label = "Select Numeric", choices = ""),
          selectInput(inputId = "scatterCategorical", label = "Select Categorical", choices = ""),
          selectizeInput(inputId = "scatterNumberSel", label = "Select 2 numbers", choices = ""),
          actionButton(inputId = "runScatter", "Create Scatterplot")
          
        ),
        conditionalPanel(
          condition = "input.plottype == 'Correlogram'",
          selectizeInput(inputId = "correlogramNumberSel", label = "Select numbers", choices = ""),
          actionButton(inputId = "runCorrelogram", "Create Correlogram")
        ),
        
        conditionalPanel(
          condition = "input.plottype == 'Histogram'",
          selectInput(inputId = "histogramCategorical", label = "Select Categorical", choices = ""),
          actionButton(inputId = "runHistogram", "Create Histogram")
        ),
        
        checkboxGroupInput(inputId = "tableFieldSelection", label = "Select Fields for Table Display")
      ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel('Table', tableOutput('contents')),
                  tabPanel('GenPlot', plotOutput('generic')),
                  tabPanel('Plot', plotOutput('plot')),
                  tabPanel('Pie', plotOutput('pie')),
                  tabPanel('Scatterplot', plotOutput('scatterplot')),
                  tabPanel('Correlogram', plotOutput('correlogram')),
                  tabPanel('Str', textOutput('str'))
    )
  )
))


server <- shinyServer(function(input, output, session) {

  
  ###  Load data   
  dataread <- eventReactive(input$file, {
    if ( is.null(input$file)) return(NULL)
    inFile <- input$file
    file <- inFile$datapath
    print(paste("File = ", file))
    read.csv(file, header=input$header, sep=input$sep, quote=input$quote)
  } )
  
  ### Once data is loaded into dataread() load filterable data into mydata()
  mydata <- eventReactive(dataread(), {
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
      cbind(colnames(dataread()),(sapply(dataread(), class)))
    }
  )
  
  ### Update all input fields when data
  observeEvent( dataread_str(), {
      updateCheckboxGroupInput(session = session, inputId = "scatterFieldSelection",  choices = colnames(dataread()), selected = colnames(dataread()))
      updateCheckboxGroupInput(session = session, inputId = "tableFieldSelection",  choices = colnames(dataread()), selected = colnames(dataread()))
      updateSelectInput(session = session, inputId = "scatterNumeric", choices = (dataread_str()[dataread_str()[,2] != "factor", 1]))
      updateSelectizeInput(session = session, inputId = "scatterNumberSel", choices = (dataread_str()[dataread_str()[,2] != "factor", 1]), options = list(maxItems = 2, minItems = 2))
      updateSelectInput(session = session, inputId = "scatterCategorical", choices = (dataread_str()[dataread_str()[,2] == "factor", 1]))
      updateSelectInput(session = session, inputId = "histogramCategorical", choices = (dataread_str()[dataread_str()[,2] == "factor", 1]))
      updateSelectizeInput(session = session, inputId = "correlogramNumberSel", choices = (dataread_str()[dataread_str()[,2] != "factor", 1]), options = list(maxItems = 99, minItems = 2))
      print(paste(input$scatterFieldSelection, "scatterFieldSelection1"))
  } )
  
  scatterEvent <- eventReactive(input$runScatter, {
    cbind(dataread()[,input$scatterCategorical],dataread()[,input$scatterNumberSel])
  })
  
  histogramEvent <- eventReactive(input$runHistogram, {
    dataread()
  })
  
  correlogramEvent <- eventReactive(input$runCorrelogram, {
    dataread()[,input$correlogramNumberSel]
  })
  
  # observe( {
  #   print(paste(input$scatterFieldSelection, "scatterFieldSelection"))
  #   print(paste(input$correlogram, "correlogramFieldSelection"))
  #   print("----------------------------------------------------------")
  #   print(dataread_str())
  #   print(correlogramEvent())
  #   print("----------------------------------------------------------")
  #   print("")
  # })
  
  output$contents <- renderTable(
    if (length(input$tableFieldSelection) == 0)
        return(NULL)
    else
        mydata()[,input$tableFieldSelection]
  )
  
  output$plot <- renderPlot(
    create_count_histogram_With_Gradient_filling(mydata(), mydata()$parental.level.of.education, "Parental Education Histogram", "XXXX", "Count")
  )

  output$pie <- renderPlot(
    create_pie_chart(mydata(), as.factor(mydata()$race.ethnicity), "Pie Chart of Ethnicity", "Source: StudentPerformance.csv")
  )
  
  output$scatterplot <- renderPlot(
    create_scatterplot_for_each_categorical_variable(mydata(), mydata()$math.score, mydata()$reading.score, as.factor(mydata()$parental.level.of.education), "Student Performance", "Maths Score", "Reading Score", "SourceLink")
  )
  
  output$correlogram <- renderPlot(
    if (length(input$scatterFieldSelection) == 0)
      return(NULL)
    else
      create_correlogram(mydata()[c(6:8)], "Correlogram of Scores")
  )
  
  output$str <- renderText(
    if (length(input$scatterFieldSelection) == 0)
      return(NULL)
    else
      dataread_str()[dataread_str()[,2] != "numeric", 1]
    
  )
  
  output$fileUploaded <- reactive({
    return(!is.null(dataread()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  output$generic <- renderPlot(
    ### pDataframe, pXvar, pyVar, pCategoricalVar, pMainTitle, pXTitle, pYtitle, pCaption
    if (input$plottype == "Scatterplot")
    {
      create_scatterplot_for_each_categorical_variable(scatterEvent(), scatterEvent()[,2], scatterEvent()[,3], scatterEvent()[,1], "X", "Y", "Z", "A")
    }
    else if (input$plottype == "Correlogram")
    {
      create_correlogram(correlogramEvent(), "Correlogram of Scores")
    }
    else if (input$plottype == "Histogram")
    {
      create_count_histogram_With_Gradient_filling(histogramEvent(), histogramEvent()[,input$histogramCategorical], "Parental Education Histogram", colnames(histogramEvent()[,input$histogramCategorical]), "Count")
    }
    else NULL
  )
  
})

shinyApp(ui = ui, server = server)

