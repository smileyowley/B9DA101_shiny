rm(list=ls())
if(!require("shiny")) {install.packages("shiny")}
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("ggcorrplot")) install.packages("ggcorrplot")
library(ggplot2)
library(ggcorrplot)
library(shiny)

shinyApp(
  ui = tagList(
    # shinythemes::themeSelector(),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "shinythemes",
      titlePanel('Parameters'),
      
      #Navbar 1
      # has sidepanel
      tabPanel("1. Descriptive Techniques",
               
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
                 
                 conditionalPanel(
                   condition = "input.plottype == 'Scatterplot'",
                   selectInput(inputId = "Numeric", label = "Select Numeric", choices = ""),
                   selectInput(inputId = "Categorical", label = "Select Categorical", choices = ""),
                   selectizeInput(inputId = "NumberSel", label = "Select 2 numbers", choices = ""),
                   actionButton(inputId = "runScatter", "Create Scatterplot"),
                   checkboxGroupInput(inputId = "FieldSelection", label = "Select Fields for Table Display")
                 )
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Table",
                            #h4("Table"),
                            #tableOutput("table"),
                            #h4("Verbatim text output"),
                            #verbatimTextOutput("txtout"),
                            #h1("Header 1"),
                            #h2("Header 2"),
                            #h3("Header 3"),
                            #h4("Header 4"),
                            #h5("Header 5")
                   ),
                   tabPanel("GenPlot", "INSERT CODE HERE AND BELOW, BEWARE OF BRACKETS"),
                   tabPanel("Plot"),
                   tabPanel("Pie"),
                   tabPanel("Scatterplot"),
                   tabPanel("Correlogram"),
                   tabPanel("Str")
                 )
               )
      ),
      
      #Navbar 2
      # currently empty sidepanel
      tabPanel("2. Probability Models",
               mainPanel(
                 tabsetPanel(
                   tabPanel("Discreet Model", "INSERT CODE HERE AND BELOW, BEWARE OF BRACKETS"),
                   tabPanel("Continuous Model", "INSERT CODE HERE AND BELOW, BEWARE OF BRACKETS")
                 )
               )
      ),
      
      #Navbar 3
      # currently empty sidepanel
      tabPanel("3. Hypothesis Testing", "INSERT CODE HERE AND BELOW, BEWARE OF BRACKETS"),
      
      #Navbar 4
      # currently empty sidepanel
      tabPanel("4. General Linear Models", "INSERT CODE HERE AND BELOW, BEWARE OF BRACKETS")
    )
  ),
  
  server = function(input, output) {
    output$txtout <- renderText({
      paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$table <- renderTable({
      head(cars, 4)
    })
  }
)

shinyApp(ui = ui, server = server)