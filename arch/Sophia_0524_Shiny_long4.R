library(shiny)
library(tidyverse)
library(openxlsx)
library(janitor)
library(stringdist)
library(lubridate)
library(tableone)

ui <- fluidPage(
  # Here everything is an argument to fluidPage(), so commas are needed between blocks
  titlePanel("Longitudinal Data Cleaning"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose XLSX/CSV File to Upload",accept=c(".xlsx",".csv")),
      textInput("varID", "Number of your ID colume", value = "1"),
      radioButtons("datatype", label = "Data Type",
                   choices = list("Visit per row" = "long", "Patient per row" = "wide"), 
                   selected = 1),
      hr(),
      textInput(inputId = "outFileName",label = "Output File Name:",value = "processed.xlsx"),
      downloadButton("downloadData", "Download Output"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Org", 
                           h4("Orginial Data"),
                           h5("This page shows your original data after a fuzzy cleaning, please select the column that indicates the patient information."),
                           DT::dataTableOutput("org")),
                  tabPanel("Summary", 
                           h4("Duplicate"),
                           h5("under development"),
                           hr(),
                           h4("Error"),
                           h5("under development"),
                           hr(),
                           textOutput("inpSummary"),
                           h4("Summary table of patients"),
                           DT::dataTableOutput("table1")),
                  tabPanel("Wide Format", 
                           DT::dataTableOutput("wild")),
                  tabPanel("Long Format", 
                           DT::dataTableOutput("long"))
      )
    )
  )
)

server <- function(input, output) {
  # each of these blocks is just a line in a function, so no commas between them
  inpD <- reactive({    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, rows will be shown.
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    if (ext=="csv") df <- read.csv(input$file1$datapath,stringsAsFactors = FALSE) else
      df <- read.xlsx(input$file1$datapath)
    # print(df)
    # dput(df)
    return(df)}  # as if stuff in {} is body of a function
  )
  
  ## after Fuzzy Clearning 
  sedD <- reactive({
    di <-inpD()
    NewDat <- FuzzyClean(di)
    return(NewDat)
  })
  
  ## first tab org data 
  output$org <- DT::renderDataTable(
    sedD(),
    selection = list(target = 'column'),
    rownames= FALSE
  )
  
  col_sel <- reactive({
    return(input$org_columns_selected)
  })
  
  ## Second tab, summary
  
  output$table1 <- DT::renderDataTable({
    di=sedD()
    var_id <- col_sel()
    table1out <- TheTable1(di[,var_id])
    table1out
  },
  rownames= FALSE)
  
  ## Third tab, wild Format 
  Dwild <- reactive({
    di <- sedD()
    var_id <- col_sel()
    id_name  <- colnames(di)[as.numeric(input$varID)]
    Dwild <- long2wide(di, ID = id_name , var_base = di[,var_id] %>%  colnames())
    return(Dwild)
  })
  
  
  output$wild <- DT::renderDataTable({
    d <- Dwild()
    d
  },
  rownames= FALSE)
  
  ## Forth tab, long Format
  
  
  
  ## Download

  output$downloadData <- downloadHandler(
    filename = function() { 
      "Dwide.csv"
    },
    content = function(file) {
      write.csv(Dwild(),file, row.names = FALSE)
    }
  )
  
  
  
}

shinyApp(ui, server) 
#killing this via the console STOP sign may be better than via the web interace (if the latter crashes R)
