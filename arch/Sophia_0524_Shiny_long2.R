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
                           DT::dataTableOutput("org")),
                  tabPanel("Summary", 
                           textOutput("inpSummary"),
                           h4("Summary table for patients"),
                           DT::dataTableOutput("table1")),
                  tabPanel("Wide Format", 
                           DT::dataTableOutput("wild"))
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
  Dtran <- reactive({
    di <- sedD()
    var_id <- col_sel()
    id_id  <- di[,input$varID] %>% colnames()
    var_base = di[,var_id] %>%  colnames()
    if (input$radio == "long"){
    Dtran <- long2wide(di, ID = id_id, var_base = var_base)
    }else
      if (input$radio == "wide"){
        Dtran <- wide2long(wide_test, ID = "patient.code", var_base = var_base, timename = "Time.", nvisit = 4)
      }
    return(Dtran)
  })
  
  
  output$wild <- DT::renderDataTable({
    d <- Dtran()
    d
  },
  rownames= FALSE)
  
  ## Forth tab, long Format
  

  
  ##
  
  
  ## Download

  output$downloadData <- downloadHandler(
    filename = function() { 
      "Dwile.xlsx"
    },
    content = function(file) {
      Dtran()
    }
  )
  
  
  
}

shinyApp(ui, server) 
#killing this via the console STOP sign may be better than via the web interace (if the latter crashes R)
