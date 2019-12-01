rm(list=ls())
if(!require("shiny")) {install.packages("shiny")}
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("ggcorrplot")) install.packages("ggcorrplot")
library(shiny)
library(ggplot2)
library(ggcorrplot)

##### CREATING THE GRAPHS #####
### Bar for categorical variables
create_count_histogram_With_Gradient_filling <- function(pDataframe, pCategoricalVar, pMainTitle, pXtitle, pYtitle){
  ggplot(data=pDataframe, aes(x = pCategoricalVar)) +
    geom_bar(
      col="red",
      aes(fill=..count..),                                     ###  Shades bars according to count
      alpha = .5                                               ###  transparency of bars 0 (fully transparent) -> 1 (fully opaque)
    ) +
    scale_fill_gradient("Count", low="green", high = "red") +  ###  alternative fill setting colours according to count
    labs(title = pMainTitle, x = pXtitle, y = pYtitle)}

### Build a pie chart
create_pie_chart <- function(pDataframe, pCategoricalVar, pMainTitle, pCaption){
  pie <- ggplot(pDataframe, aes(x = "", fill = pCategoricalVar)) + 
    geom_bar(width = 1) +
    theme(axis.line = element_blank(), 
          plot.title = element_text(hjust=0.5)) + 
    labs(fill="weight", 
         x=NULL, 
         y=NULL, 
         title=pMainTitle, 
         caption=pCaption)
  pie + coord_polar(theta = "y", start=0)}

### Stacked Scatterplot
create_scatterplot_for_each_categorical_variable <- function(pDataframe, pXvar, pyVar, pCategoricalVar, pMainTitle, pXTitle, pYtitle, pCaption){
  ggplot(pDataframe, aes(x = pXvar, y = pyVar, col = pyVar)) +
    geom_point() +
    facet_grid(rows = pCategoricalVar, scales = "free") +                                ## With facet_grid individual graph by Diet otherwise not broken down
    labs(title = pMainTitle, x = pXTitle, y = pYtitle, caption = pCaption) +
    stat_smooth(method = "lm", col = "red", se = FALSE)}

create_correlogram  <- function(pContinuousVar, pTitle){
  corr <- round(cor(pContinuousVar),1)
  ggcorrplot(corr, hc.order = TRUE,
             type = "full",
             lab = TRUE,
             lab_size = 4,
             method = "circle",
             colors = c("tomato2", "white", "springgreen3"), 
             title=pTitle, 
             ggtheme=theme_classic())}

shinyApp(
  
  ### UI CODE ###
  ui = tagList(
    # shinythemes::themeSelector(),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "shinythemes",
      
      #Navbar 1
      # has sidepanel
      tabPanel("1. Descriptive Techniques",
          headerPanel("Parameters"),
          # Sidebar panel for inputs ----
          sidebarPanel(
                 
            # Input: Slider for the number of bins ----
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
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            conditionalPanel(condition = "output.fileUploaded",
                                  radioButtons(inputId = "plottype", label = "Select Plot Type", choices = c("Scatterplot", "Correlogram", "Histogram"), selected = "")),
                 
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
      ),
      
      #Navbar 2
      # currently empty sidepanel
      tabPanel("2. Probability Models",
          headerPanel("Parameters"),
          mainPanel(
            tabsetPanel(
              tabPanel("Discreet Model", "INSERT CODE HERE AND BELOW, BEWARE OF BRACKETS"),
              tabPanel("Continuous Model", "INSERT CODE HERE AND BELOW, BEWARE OF BRACKETS")
        )
        )
      ),
      
      #Navbar 3: Hypothesis Testing
      tabPanel("3. Hypothesis Testing", 
          headerPanel("Parameters"), 
          sidebarPanel(selectInput("ds", "Data Source :",
                      c("File" = "file",
                      "URL"= "url")),
            
            # Input: URL   
            conditionalPanel(    
                condition = "input.ds == 'url'",
                textInput("url", label = "Input URL", value = "http://www.stat.ufl.edu/~winner/data/nfl2008_fga.csv")
                 ),
                 
            # Input: File
            conditionalPanel(    
                condition = "input.ds == 'file'",
                fileInput("datafile", "Choose CSV File",
                             multiple = FALSE,
                             accept = c("text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv"))
                 ),
                 
                 selectInput(inputId = "columns", label = "Select a Column", choices = ""),
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
               
               mainPanel( 
                 #tableOutput("table"),
                 #h4("Verbatim text output"),
                 #verbatimTextOutput("txtout"),
                 #h1("Header 1"),
                 #h2("Header 2"),
                 #h3("Header 3"),
                 #h4("Header 4"),
                 #h5("Header 5")
                 #tableOutput('prob'), # Shows chosen column + decision
                 DT::dataTableOutput('extdata'),
                 #DT::dataTableOutput('summary'),
                 #verbatimTextOutput("prt")
               )
      ),
      
      #NAVBAR 4: GLM
      # currently empty sidepanel
      tabPanel("4. General Linear Models",
          headerPanel("Parameters"),)
    )
  ),

  
  ### SERVER CODE ###
  server = function(input, output, session){
    
    ##### NAVBAR 1 SERVER-SIDE #####
    ### dataread <- reactive( { read.csv(input$file1, header=input$header, sep=input$sep, quote=input$quote) } )
    ### dataread <- eventReactive(input$file1, { read.csv(input$file1, header=input$header, sep=input$sep, quote=input$quote) } )
    
    cols <- NULL
    dataread <- eventReactive(input$file, {
      if ( is.null(input$file)) return(NULL)
      inFile <- input$file
      file <- inFile$datapath
      print(paste("File = ", file))
      read.csv(file, header=input$header, sep=input$sep, quote=input$quote)
    })
    
    mydata <- eventReactive(dataread(), {
      if (is.null(dataread()))
      {
        print("NULL mydata")
        return(NULL)
      }
      dataread()
    })
    
    dataread_str <- eventReactive(dataread(), {
      cbind(colnames(dataread()),(sapply(dataread(), class)))
    })
    
    observeEvent( dataread(), {
      updateCheckboxGroupInput(session = session, inputId = "FieldSelection",  choices = colnames(dataread()), selected = colnames(dataread()))
      updateSelectInput(session = session, inputId = "Numeric", choices = (dataread_str()[dataread_str()[,2] != "factor", 1]))
      updateSelectizeInput(session = session, inputId = "NumberSel", choices = (dataread_str()[dataread_str()[,2] != "factor", 1]), options = list(maxItems = 2, minItems = 2))
      updateSelectInput(session = session, inputId = "Categorical", choices = (dataread_str()[dataread_str()[,2] == "factor", 1]))
      ###print(paste(input$FieldSelection, "FieldSelection1"))
    })
    
    scatterEvent <- eventReactive(input$runScatter, {
      cbind(mydata()[,input$Categorical],mydata()[,input$NumberSel])
    })
    
    observe( {
      print(paste(input$FieldSelection, "FieldSelection"))
      print("----------------------------------------------------------")
      print(dataread_str())
      print("----------------------------------------------------------")
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
    
    output$str <- renderText(
      if (length(input$FieldSelection) == 0)
        return(NULL)
      else
        dataread_str()[dataread_str()[,2] != "numeric", 1]
      
    )
    
    output$fileUploaded <- reactive({
      return(!is.null(dataread()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    output$generic <- renderPlot(
      ### pDataframe, pXvar, pyVar, pCategoricalVar, pMainTitle, pXTitle, pYtitle, pCaption
      create_scatterplot_for_each_categorical_variable(scatterEvent(), scatterEvent()[,2], scatterEvent()[,3], scatterEvent()[,1], "X", "Y", "Z", "A")
    )
    
    
    ##### NAVBAR 3 SERVER-SIDE #####
    myData1 <- reactive({
      if (input$ds == 'file') { 
        file1 <- input$datafile
        if (is.null(file1)) { 
          return() 
        } 
        data = read.csv(file=file1$datapath)
        data
      }
    })
    
    myData2 <- reactive({
      
      if (input$ds == 'url')
      {
        data = read.csv(file=input$url)
        data
      }
    })
    
    observe({
      switch(input$ds,
             file = { 
               updateSelectInput(session, "columns",
                                 choices = colnames(myData1()))
             },
             url =   { 
               updateSelectInput(session, "columns",
                                 choices = colnames(myData2()))
             }
      )
    })
    
    'output$extdata = DT::renderDataTable({
      switch(input$ds, 
             file = { extdata <- myData1() },
             url =  { extdata <- myData2() }
      )
      DT::datatable(extdata, options = list(lengthChange = TRUE))
    })'
    
    #TableOutput prob here
    output$prob <- renderPrint({
      
      print(paste('Selected Column :',input$columns))
      
      switch(input$ds, 
             file = { df <- myData1() },
             url  = { df <- myData2() }
      )
      df=na.omit(df)
      
      x <- df[,input$columns]
      if (!is.null(x))  
      {
        # Test the Mean
        conf_level = (1-as.numeric(input$alpha)) 
        t=t.test(x,mu= input$mu, alternative= input$tail,conf.level=conf_level)  
        
        alpha <- as.numeric(input$alpha)
        p_value <- as.numeric(t$p.value)
        
        if(p_value < alpha){ 
          decision='Reject H_0'}
        else{ 
          decision='Accept H_0' 
        } 
        
        print(paste(conf_level, ' The decision is to ', decision))
      }
      
      L <- mean(x)-abs(qnorm(alpha/2))*sd(x)/sqrt(length(x)) 
      U <- mean(x)+abs(qnorm(alpha/2))*sd(x)/sqrt(length(x)) 
      conf_Inter <-  c(L, U) 
      print(paste('Confidence Interval Lower :', L,'Upper :', U)) 
    })
  }
)

shinyApp(ui = ui, server = server)