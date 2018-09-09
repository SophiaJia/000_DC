library(shiny)
library(tidyverse)
library(openxlsx)
library(janitor)
library(stringdist)
library(lubridate)
library(tableone)
library(shinyBS)
library(shinyjs)
library(DT)
library(plotly)
library(readxl)

ui <- fluidPage(
  # Here everything is an argument to fluidPage(), so commas are needed between blocks
  titlePanel("Understand Your Data"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose XLSX/CSV File to Upload",accept=c(".xlsx",".csv")),
      hr(),
      textInput("varID", "Number of your ID colume", value = "1"),
      radioButtons("datatype", label = "Data Type",
                   choices = list("Patient per row（Default）" = "wide","Visit per row" = "long"),
                   selected = 1),
      hr(),
      #textInput(inputId = "outFileName",label = "Output File Name:",value = "processed.xlsx"),
      checkboxGroupInput("download_check", "Download",
                         choices = list("Clean Data" = 1, "Dirty/Clean Compare" = 2, "Transformed Data" = 3, "Table 1" = 4),
                         selected = 1),
      downloadButton("downloadData", "Download Output"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Instruction",
                           h5("Step one")
                  ),
                  tabPanel("Org Data",
                           dataTableOutput("org")),
                  tabPanel("After Fuzzy Cleaning",
                           dataTableOutput("fuzzy")),
                  tabPanel("Continuous",
                           h4("Summary table"),
                           h5("This page summarizes the continuous variable of your data. If you think any range is incorrect, check it manually."),
                           dataTableOutput("inpContinuous"),
                           h4("Data Checking (Select the variable)"),
                           htmltools::div(style = "display:inline-block", plotlyOutput("con_hist", width = 540, height = 200)),
                           wellPanel(
                             style = "display:inline-block; vertical-align:bottom;",
                             sliderInput("xbins", "Number of x bins",
                                         min = 1, max = 50, value = 20, width = 230)
                           ),
                           textInput("con_min", "Minimum value", value = "0"),
                           textInput("con_max", "Maximum Value", value = "100"),
                           actionButton("con_check", label = "Check")
                           
                  ) ,
                  tabPanel("Categorical",
                           h4("1.Variables with one level, recommend remove"),
                           h5("These variables only have one layer. It might because 1) all of the patients have the same value, in which case you can remove this column; 2) it is non-random missing, in which case you should check your data."),
                           dataTableOutput("inpCategorical1"),
                           hr(),
                           h4("2.Variables with 2-4 levels, check correctness"),
                           h5("These variables have 2 - 4 levels, which is suitable for analysis. However, you still need to check if there are typos and make sure that the level number is correct. E.g., gender should has only two levels. "),
                           dataTableOutput("inpCategorical234"),
                           hr(),
                           h4("3.Variables more than 5 levels, recommend clean up"),
                           h5("These variables have more than four levels. It usually is not suitable for analysis. We recommend you to check if to and try your best to compress less than four. "),
                           dataTableOutput("inpCategorical5")),
                  tabPanel("Date",
                           h4("Date Summary"),
                           h4("Reorder the date column"),
                           dataTableOutput("inpDate"),
                           actionButton("date_exchage", label = "Reorder"),
                           actionButton("date_notshow", label = "Hide"),
                           actionButton("date_reset", label = "Reset"),
                           actionButton("date_check",   label = "Check"),
                           h4("Something wrong with the date"),
                           dataTableOutput("date_wrong_out")
                  ),
                  tabPanel("Cleaned Data",
                           h5("If your data is repeated measure and has per visit per row,choose the column that indicates patient characteristics"),
                           dataTableOutput("afterclean")
                  ),
                  tabPanel("Wide Type Data",
                           h5("This is for repeated measure data only"),
                           dataTableOutput("wide")),
                  tabPanel("Summary",
                           h4("Summary"),
                           textOutput("inpSummary"),
                           hr(),
                           #h4("Exact Duplicate"),
                           #dataTableOutput("dup"),
                           #hr(),
                           h4("Table 1"),
                           dataTableOutput("table1"),
                           h5("We made some default changes to your data; please download for more details."))
      )
    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  options(DT.options = list(pageLength = 15))
  ###0 input                                                               ####
  inpD <- reactive({
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    if (ext=="csv"){
      Org <- read.csv(input$file1$datapath,stringsAsFactors = FALSE)
    } else {
      Org <- read.xlsx(input$file1$datapath)
    }
    return(Org)
  }
  )
  
  outD <- reactive({
    Org=inpD()
    id  <- as.numeric(input$varID)
    Org <- Org[!Org[,id] == "",]# If ID = "" then delete this row
    #Org <- Org[!is.na(Org[,id])]
    NewDat <- FuzzyClean2(Org)
    return(NewDat)
  })
  
  
  ###1 org data ####
  output$org <- renderDataTable({
    di <- inpD()
    di
  },
  selection = list(target = 'column'),
  rownames= FALSE)
  
  ###2 fuzzy data ####
  output$fuzzy <- renderDataTable({
    di <-outD()
    di
  },
  selection = list(target = 'column'),
  rownames= FALSE)
  
  #con
  
  
  
  #cat
  
  #date
  
  #summary 
  
}




shinyApp(ui, server)

