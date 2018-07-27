library(shiny)
library(tidyverse)
library(openxlsx)
library(janitor)
library(lubridate)
#library(scRub)

ui <- fluidPage(
  # Here everything is an argument to fluidPage(), so commas are needed between blocks
  titlePanel("Data Checking"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose XLSX/CSV File to Upload",accept=c(".xlsx",".csv")),
      textInput(inputId = "outFileName",label = "Output File Name:",value = "processed.xlsx"),
      downloadButton("downloadData", "Download Output"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summary", 
                           h4("Summary"),
                           verbatimTextOutput("inpSummary")),
                  tabPanel("Continuous", tableOutput("inpContinuous")) ,
                  tabPanel("Categorical", 
                           h2("Variables with one level, recommend remove"),
                           tableOutput("inpCategorical1"),
                           h2("Variables with 2-4 levels, check correctness"),
                           tableOutput("inpCategorical234"),
                           h2("Variables more than 5 levels, recommend clean up"),
                           tableOutput("inpCategorical5")),
                  tabPanel("Date", tableOutput("inpDate")),
                  tabPanel("Other", verbatimTextOutput("inpOther"))
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
  
    
  output$inpSummary <- renderPrint({
    d1 <- inpD()
    rbind(c("Number of Patient:", length(d1[,1])))
    
  })
  
  output$inpContinuous <- renderTable({
    di <- outD()
    d_con <- di[sapply(di, mode) == "numeric" & sapply(di, class) != "Date"]
    
    contable <- cbind(
      colnames(d_con),
      apply(d_con, 2, min, na.rm=TRUE),
      apply(d_con, 2, max, na.rm=TRUE)
    )
    colnames(contable) <- c("Variable Name","Minimum","Maximum")
    contable
    
  })
  
  output$inpCategorical <- renderTable({
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
  
  output$inpCategorical1 <- renderTable({
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
  
  output$inpCategorical234 <- renderTable({
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
  
  output$inpCategorical5 <- renderTable({
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

  output$inpDate <- renderTable({
    di <- outD()
    d_date<- di[sapply(di, class) == "Date"]
    datetable <- cbind(
      colnames(d_date),
      apply(d_date, 2, min, na.rm=TRUE),
      apply(d_date, 2, max, na.rm=TRUE)
    )
    colnames(datetable) <- c("Variable Name","First Day","Last Day")
    datetable
  })
  
  output$inpOther <- renderPrint({
    dc=outD()
    (nm=names(dc))
    L=vector("list",length=length(nm))
    names(L)=nm
    for (i in nm) {
      D=tabyl(as.character(dc[[i]]),sort=T)[1:3]
      names(D)[1]="value"
      L[[i]]=D
    } 
    print(L) # summary of entries
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
      mkReport2(inpD(),outD(),f=file)
    }
  )
  
  
  
}

shinyApp(ui, server) 
#killing this via the console STOP sign may be better than via the web interace (if the latter crashes R)
