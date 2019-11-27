rm(list=ls())
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
        checkboxGroupInput(inputId = "FieldSelection", label = "Select Fields for Table Display")
        
      ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel('Table', tableOutput('contents')),
                  tabPanel('Plot', plotOutput('plot')),
                  tabPanel('Pie', plotOutput('pie')),
                  tabPanel('Scatterplot', plotOutput('scatterplot')),
                  tabPanel('Correlogram', plotOutput('correlogram'))
    )
  )
))


server <- shinyServer(function(input, output, session) {
  ### dataread <- reactive( { read.csv(input$file1, header=input$header, sep=input$sep, quote=input$quote) } )
  ### dataread <- eventReactive(input$file1, { read.csv(input$file1, header=input$header, sep=input$sep, quote=input$quote) } )
  
  cols <- NULL
  
  dataread <- eventReactive(input$file, {
    if ( is.null(input$file)) return(NULL)
    inFile <- input$file
    file <- inFile$datapath
    print(paste("File = ", file))
    read.csv(file, header=input$header, sep=input$sep, quote=input$quote)
  } )
  
  mydata <- eventReactive(dataread(), {
    if (is.null(dataread()))
    {
      print("NULL mydata")
      return(NULL)
    }
    dataread()
  })
  
  observeEvent( dataread(), {
      print(ncol(dataread()))
      ## updateCheckboxGroupInput(session = session, inputId = "FieldSelection", choiceValues = c(1:ncol(dataread())),  choiceNames = colnames(dataread()), selected = c(1:ncol(dataread())))
      updateCheckboxGroupInput(session = session, inputId = "FieldSelection",  choices = colnames(dataread()), selected = colnames(dataread()))
      print(paste(input$FieldSelection, "FieldSelection1"))
  } )
  
  
  observe( {
    print(paste(input$FieldSelection, "FieldSelection"))
    print("")
  })
  
  output$contents <- renderTable(
    if (length(input$FieldSelection) == 0)
        return(NULL)
    else
        mydata()[,input$FieldSelection]
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
    if (length(input$FieldSelection) == 0)
      return(NULL)
    else
      create_correlogram(mydata()[c(6:8)], "Correlogram of Scores")
  )
})

shinyApp(ui = ui, server = server)

