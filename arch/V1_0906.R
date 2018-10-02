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

## some of the fuctions 
movetolast <- function(data, move) {
  data[c(move,setdiff(names(data), move))]
}

allLevel <- function(x){
  x  %>% table %>% names %>% paste(., collapse="; ")
}

ui <- fluidPage(
  # Here everything is an argument to fluidPage(), so commas are needed between blocks
  titlePanel("Understand Your Data"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose XLSX/CSV File to Upload",accept=c(".xlsx",".csv")),
      hr(),
      textInput("varID", "Index of your ID colume", value = "1"),
      hr(),
      #textInput(inputId = "outFileName",label = "Output File Name:",value = "processed.xlsx"),
      checkboxGroupInput("download_check", "Download",
                         choices = list("Fuzzy cleaned Data" = 1, "Summary Table" = 2),
                         selected = 1),
      downloadButton("downloadData", "Download Output"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Org Data",
                           dataTableOutput("org")),
                  tabPanel("After Fuzzy Cleaning",
                           dataTableOutput("fuzzy")),
                  tabPanel("Continuous",
                           h3("Summary Table (Select the variable (row))"),
                           h5("This page summarizes the continuous variable of your data."),
                           dataTableOutput("inpContinuous"),
                           hr(),
                           h3("Visualization"),
                           h4("Histogram"),
                           htmltools::div(style = "display:inline-block", plotlyOutput("con_hist", width = 540, height = 200)),
                           wellPanel(
                             style = "display:inline-block; vertical-align:bottom;",
                             sliderInput("xbins", "Number of x bins",
                                         min = 1, max = 50, value = 20, width = 230)
                           ),
                           h4("Box Plot"),
                           plotlyOutput("con_box", width = 540, height = 200),
                           h3("  "),
                           hr(),
                           h3("Data Checking"),
                           textInput("con_min", "Minimum value", value = "0"),
                           textInput("con_max", "Maximum Value", value = "100"),
                           dataTableOutput("con_aftercheck")
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
    inpD()
  },
  selection = list(target = 'column'),
  rownames= FALSE)
  
  ###2 fuzzy data ####
  output$fuzzy <- renderDataTable({
    outD()
  },
  selection = list(target = 'column'),
  rownames= FALSE)
  
  ###3 con var ####
  
  ## make a con var dataset 
  
  con <- reactive({
    d <- outD()
    d [sapply(d, class) == "numeric" | sapply(d, class) == "integer"]
  })
  
  output$inpContinuous <- renderDataTable({
    d <- con()
    contable <- cbind(
      #`ID` = c(1:length(colnames(d_con))),
      #`Variable name` = colnames(d_con),
      `Mean`       = sapply(d, mean, na.rm=TRUE) %>% round(digits = 2),
      `Median`     = apply(d, 2, median, na.rm=TRUE),
      `Minimum`    = apply(d, 2, min, na.rm=TRUE),
      `Maximum`    = apply(d, 2, max, na.rm=TRUE),
      `No.Missing` = apply(d, 2,function(x) sum(is.na(x)))
    )
    contable
  },selection = 'single')
  
  # histogram with bins
  # convenience function for computing xbin/ybin object given a number of bins
  compute_bins <- function(x, n) {
    list(
      start = min(x, na.rm = T),
      end = max(x, na.rm = T),
      size = (max(x, na.rm = T) - min(x, na.rm = T)) / n
    )
  }
  
  # marker objects (con hist)
  m <- list(color = toRGB("black"))
  m2 <- list(color = toRGB("black", 0.2),
             line = list(color = 'black', width = 0.3))
  
  # the histogram
  output$con_hist <- renderPlotly({
    d <- con()
    index <- input$inpContinuous_rows_selected
    if(is.null(input$inpContinuous_rows_selected)){
      index = as.numeric(input$varID)
    }
    
    names <- colnames(d)[index]
    x  <-d[,index]
    xbins <- compute_bins(x, input$xbins)
    p <- plot_ly(x = x, type = "histogram", autobinx = F,
                 xbins = xbins, marker = m2)
    # obtain plotlyjs selection
    s <- event_data("plotly_selected")
    # if points are selected, subset the data, and highlight
    if (length(s$x) > 0) {
      p <- add_trace(p, x = s$x, type = "histogram", autobinx = F,
                     xbins = xbins, marker = m)
    }
    p %>%
      config(displayModeBar = F, showLink = F) %>%
      layout(showlegend = F, barmode = "overlay", yaxis = list(title = "count"),
             xaxis = list(title = names , showticklabels = T))
  })
  
  # box plot (later)
  output$con_box <- renderPlotly({
    d <- con()
    index <- input$inpContinuous_rows_selected
    if(is.null(input$inpContinuous_rows_selected)){
      index = as.numeric(input$varID)
    }
    
    names <- colnames(d)[index]
    x  <-d[,index]
    xbins <- compute_bins(x, input$xbins)
    p <- plot_ly(x = x, type = "box", boxpoints = "all", jitter = 0.3,pointpos = -1.8)
  })
  # check the range 
  
  con_check_data = reactiveValues(
    datarow = 2222
  )
  
  toListen <- reactive({
    list(input$con_min,input$con_max,input$inpContinuous_rows_selected)
  })
  
  observeEvent(toListen(),{
    d <- con()
    
    # `tmp ID` <-seq.int(nrow(d))
    # d_con  <- cbind(d,`tmp ID`)  ## this is for editing will not need it now. 
    
    var_index <- input$inpContinuous_rows_selected # the index number of con
    var_names <- colnames(d)[var_index]   # var name 
    id_names  <- colnames(d)[as.numeric(input$varID)]
    di <- outD()  # the whole table 
    
    out  <- di[(di[,var_names] < as.numeric(input$con_min) | di[,var_names] > as.numeric(input$con_max)) &!is.na(di[,var_names]),]
    out3 <- movetolast(out, c(id_names,var_names))
    con_check_data[['table']] = out3
  })
  
  # make it the DT output
  output$con_aftercheck<-DT::renderDataTable({
    con_check_data[['table']]
  },
  selection = 'none',
  filter = 'top',
  rownames = FALSE,
  extensions = 'Buttons', 
  options = list(
    dom = 'B',
    buttons = list(list(extend = 'colvis')),
    columnDefs = list(
      list(targets = {c(0:1)}, visible = TRUE),
      list(targets = {'_all'}, visible = FALSE)
    )
  )
  )
  
  
  ###4 cat var ####
  output$inpCategorical1 <- renderDataTable({
    di <- outD()
    lev <- sapply(di[sapply(di, mode) == "character"| sapply(di, class) == "factor"], table)
    lev2<- mapply(length,lev)
    leveleq1 <- lev2[lev2 == 1] %>% names
    
    d_cat <- di[leveleq1]
    
    
    ta <- cbind(
      `Variable Name`  = colnames(d_cat),
      `Levels` = apply(d_cat, 2, allLevel),
      `Missing No` =  apply(d_cat, 2,function(x) sum(is.na(x))))
    rownames(ta) <- NULL
    ta
    
  }, escape=FALSE,
  options = list(sDom  = '<"top">lrt<"bottom">ip'))
  
  output$inpCategorical234 <- renderDataTable({
    di <- outD()
    lev <- sapply(di[sapply(di, mode) == "character"| sapply(di, class) == "factor"], table)
    lev2<- mapply(length,lev)
    leveleq2 <- lev2[lev2< 5& lev2 > 1] %>% names
    
    d_cat <- di[leveleq2]
    
    ta <- cbind(
      `Variable Name`  = colnames(d_cat),
      `Levels` = apply(d_cat, 2, allLevel),
      `Missing No` =  apply(d_cat, 2,function(x) sum(is.na(x))))
    rownames(ta) <- NULL
    ta
    
  }, escape=FALSE,
  options = list(sDom  = '<"top">lrt<"bottom">ip'))
  
  
  #date
  
  #summary 
  output$table1 <- renderDataTable({
    di=outD()
    table1out <- TheTable1(di)
    table1out
  }, rownames= FALSE,
  extensions = 'Scroller', options = list(
    deferRender = TRUE,
    scrollY = 400,
    scroller = TRUE)
  )
  
}




shinyApp(ui, server)

