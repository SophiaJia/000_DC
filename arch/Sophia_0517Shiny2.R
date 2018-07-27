library(shiny)
library(tidyverse)
library(openxlsx)
library(janitor)
library(stringdist)
library(lubridate)
library(tableone)

#library(scRub)

ui <- fluidPage(
  # Here everything is an argument to fluidPage(), so commas are needed between blocks
  titlePanel("Understand Your Data"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose XLSX/CSV File to Upload",accept=c(".xlsx",".csv")),
      textInput("varID", "Number of your ID colume", value = "1"),
      textInput(inputId = "outFileName",label = "Output File Name:",value = "processed.xlsx"),
      downloadButton("downloadData", "Download Output"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", 
                           h3("Summary"),
                           textOutput("inpSummary"),
                           h3("Exact Duplicate"),
                           DT::dataTableOutput("dup"),
                           h3("We made some default changes to your data; please download for more details.")),
                  tabPanel("Continuous", 
                           h4("Summary table"),
                           h5("This page summarizes the continuous variable of your data. If you think any range is incorrect, check it manually."),
                           DT::dataTableOutput("inpContinuous"),
                           h4("Data Checking"),
                           selectInput("concheck", "Variable ID", choices = c(1:27)),
                           textInput("conmin", "Minimum value", value = "1"),
                           textInput("commax", "Maximum value", value = "100"),
                           DT::dataTableOutput("feedContinuous")
                           #actionButton('insertBtn', 'Insert'), 
                           #actionButton('removeBtn', 'Remove'), 
                           #tags$div(id = 'placeholder')
                  ) ,
                  tabPanel("Categorical", 
                           h4("1.Variables with one level, recommend remove"),
                           h5("These variables only have one layer. It might because 1) all of the patients have the same value, in which case you can remove this column; 2) it is non-random missing, in which case you should check your data."),
                           DT::dataTableOutput("inpCategorical1"),
                           h4("2.Variables with 2-4 levels, check correctness"),
                           h5("These variables have 2 - 4 levels, which is suitable for analysis. However, you still need to check if there are typos and make sure that the level number is correct. E.g., gender should has only two levels. "),
                           DT::dataTableOutput("inpCategorical234"),
                           h4("3.Variables more than 5 levels, recommend clean up"),
                           h5("These variables have more than four levels. It usually is not suitable for analysis. We recommend you to check if to and try your best to compress less than four. "),
                           DT::dataTableOutput("inpCategorical5")),
                  tabPanel("Date", 
                           h4("Date Summary"),
                           DT::dataTableOutput("inpDate"),
                           h5("If one of your endpoint is time-to-event variable, pleach check below:"),
                           selectInput("startday", "Start Day Var ID", choices = c(1:27)),
                           selectInput("lastday", "Last Day Var ID", choices = c(1:27)),
                           h5("Following this the start day later than the last day or the duration is too long"),
                           textInput("eevent", "Event Variable Name", value = "Status"),
                           textInput("eevent", "Event Date", value = "date.death"),
                           h5("Following is something wrong")
                  ),
                  tabPanel("Whole Data", DT::dataTableOutput("inpall")),
                  tabPanel("Table one", DT::dataTableOutput("table1"))
      )
    )
  )
)

server <- function(input, output) {
  options(shiny.maxRequestSize=30*1024^2)
  # each of these blocks is just a line in a function, so no commas between them
  inpD <- reactive({    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, rows will be shown.
    
    # data intput
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    if (ext=="csv"){
      Org <- read.csv(input$file1$datapath,stringsAsFactors = FALSE)
    } else {
      Org <- read.xlsx(input$file1$datapath)
    }
    
    return(Org)
    
  }  # as if stuff in {} is body of a function
  )
  
  outD <- reactive({
    Org=inpD()
    
    # data clean ##############
    NewDat <- FuzzyClean(Org)
    return(NewDat)
  })
  
  output$dup <- renderDataTable(
    {
      di <- outD()
      tmp3 <- di[duplicated(di),]
      tmp3
    }
  )
  
  
  output$inpSummary <- renderText({
    d1 <- inpD()
    rbind(c("Number of Patient:", length(d1[,1])))
    
  })
  
  output$inpContinuous <- DT::renderDataTable({
    di <- outD()
    d_con <- di[sapply(di, mode) == "numeric" & sapply(di, class) != "Date"]
    
    contable <- cbind(
      #`ID` = c(1:length(colnames(d_con))),
      #`Variable name` = colnames(d_con),
      #`Mean` = apply(d_con, 2, mean, na.rm=TRUE),
      `Median` = apply(d_con, 2, median, na.rm=TRUE),
      `Minimum` = apply(d_con, 2, min, na.rm=TRUE),
      `Maximum` = apply(d_con, 2, max, na.rm=TRUE)
    )
    contable
  })
  
  output$feedContinuous <- DT::renderDataTable({
    con_check <- as.numeric(input$concheck)
    con_min <- as.numeric(input$conmin)
    con_max <- as.numeric(input$conmax)
    idd <- as.numeric(input$varID)
    di <- outD()
    d_con <- di[sapply(di, mode) == "numeric" & sapply(di, class) != "Date"]
    
    sel_con<-
      d_con %>% 
      select(colnames(d_con)[idd], colnames(d_con)[con_check]) 
    
    sel_con[sel_con[,2]<con_min|sel_con[,2]> con_max,]
    
  })
  
  
  output$inpCategorical <- DT::renderDataTable({
    di <- outD()
    d_cat <- di[sapply(di, mode) == "character"]
    
    allLevel <- function(x){
      x  %>% table %>% names %>% paste(., collapse="; ") 
    } 
    ta <- cbind(
      `Variable Name`  = colnames(d_cat),
      `Levels` = apply(d_cat, 2, allLevel))
    rownames(ta) <- NULL
    ta
    
  })
  
  output$inpCategorical1 <- DT::renderDataTable({
    di <- outD()
    lev <- sapply(di[sapply(di, mode) == "character"], table)
    lev2<- mapply(length,lev)
    leveleq1 <- lev2[lev2 == 1] %>% names
    
    d_cat <- di[leveleq1]
    
    allLevel <- function(x){
      x  %>% table %>% names %>% paste(., collapse="; ") 
    } 
    ta <- cbind(
      `Variable Name`  = colnames(d_cat),
      `Levels` = apply(d_cat, 2, allLevel))
    rownames(ta) <- NULL
    ta
    
  })
  
  output$inpCategorical234 <- DT::renderDataTable({
    di <- outD()
    lev <- sapply(di[sapply(di, mode) == "character"], table)
    lev2<- mapply(length,lev)
    leveleq2 <- lev2[lev2< 5& lev2 > 1] %>% names
    
    d_cat <- di[leveleq2]
    
    allLevel <- function(x){
      x  %>% table %>% names %>% paste(., collapse="; ") 
    } 
    ta <- cbind(
      `Variable Name`  = colnames(d_cat),
      `Levels` = apply(d_cat, 2, allLevel))
    rownames(ta) <- NULL
    ta
    
  })
  
  output$inpCategorical5 <- DT::renderDataTable({
    di <- outD()
    lev <- sapply(di[sapply(di, mode) == "character"], table)
    lev2<- mapply(length,lev)
    leveleq2 <- lev2[ lev2 > 4] %>% names
    
    d_cat <- di[leveleq2]
    
    allLevel <- function(x){
      x  %>% table %>% names %>% paste(., collapse="; ") 
    } 
    ta <- cbind(
      `Variable Name`  = colnames(d_cat),
      `Levels` = apply(d_cat, 2, allLevel))
    rownames(ta) <- NULL
    ta
    
  })
  
  output$inpDate <- DT::renderDataTable({
    di <- outD()
    d_date<- di[sapply(di, class) == "Date"]
    datetable <- cbind(
      #`ID` = c(1:length(colnames(d_date))),
      #`Variable Name` = colnames(d_date),
      `First Day` = apply(d_date, 2, min, na.rm=TRUE),
      `Last Day` = apply(d_date, 2, max, na.rm=TRUE)
    )
    datetable
  })
  
  output$inpall <- DT::renderDataTable({
    di=outD()
    di
  })
  
  output$table1 <- DT::renderDataTable({
    di=outD()
    table1out <- TheTable1(di)
    table1out
  })
  
  # output$downloadData <- downloadHandler(
  #   filename = function() { #nesting in a function => reactivity can work?
  #     "scrubbedData.xlsx"
  #     # input$outFileName
  #   },
  #   content = function(file) {
  #     write.xlsx(outD(), file, row.names = FALSE)
  #   }
  # )
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      "scRubOut.xlsx"
    },
    content = function(file) {
      di=inpD()
      do=outD()
      reporttmp(di, do)
    }
  )
  
  
  
}

shinyApp(ui, server) 
#killing this via the console STOP sign may be better than via the web interace (if the latter crashes R)

