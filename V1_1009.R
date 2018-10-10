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
require(gdata)

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
      h5("Download"),
      downloadButton("downloadData1", "Fuzzy cleaned Data"),
      h5(" "),
      downloadButton("downloadData2", "Table1"),
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
                           dataTableOutput("con_aftercheck"),
                           br(),
                           actionButton("con_download", label = "download"),
                           br(),
                           hr()
                  ) ,
                  tabPanel("Categorical",
                           h4("I.Variables with one level, recommend remove"),
                           h5("These variables only have one layer. It might because 1) all of the patients have the same value, in which case you can remove this column; 2) it is non-random missing, in which case you should check your data."),
                           dataTableOutput("inpCategorical1"),
                           hr(),
                           h4("II.Variables with 2-4 levels, check correctness"),
                           h5("These variables have 2 - 4 levels, which is suitable for analysis. However, you still need to check if there are typos and make sure that the level number is correct. E.g., gender should has only two levels. "),
                           dataTableOutput("inpCategorical234"),
                           plotlyOutput("cat_bar234"),
                           hr(),
                           h4("III.Variables more than 5 levels, recommend clean up"),
                           h5("These variables have more than four levels. It usually is not suitable for analysis. We recommend you to check if to and try your best to compress less than four. "),
                           dataTableOutput("inpCategorical5"),
                           plotlyOutput("cat_bar5"),
                           hr()),
                  
                  tabPanel("Date",
                           h3("Check the format of the dates"),
                           h5("This part is to check the format of your date.You need to keep the date format consistent. If nothing shows up means you are good to go."),
                           dataTableOutput("wrongformDate"),
                           #actionButton("date_fix", label = "Fix"),
                           actionButton("date_download1", label = "download"),
                           hr(),
                           h3("Check the quality of the dates"),
                           h5("This part is to check the quality of your date. Please follow the steps"),
                           h5("Step 1: Select the columns that you are not interesed in, then hit the Hide button"),
                           h5("Step 2: Reorder the reminding column in a desired time order: select TWO columns and hit the Reorder button"),
                           h5("Step 3: Hit the Check button, it will show the results."),
                           br(),
                           actionButton("date_exchage", label = "Reorder"),
                           actionButton("date_notshow", label = "Hide"),
                           actionButton("date_reset", label = "Reset"),
                           actionButton("date_check",   label = "Check"),
                           dataTableOutput("inpDate"),
                           hr(),
                           h4("Something wrong with the date"),
                           h5("This table highlights the date looks wrong."),
                           dataTableOutput("date_wrong_out"),
                           br(),
                           actionButton("date_download2", label = "download"),
                           br(),
                           hr()
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
      Org <- read.xls(input$file1$datapath,sheet = 1, header = TRUE)
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
  
  #######################
  # cat 2,3,4
  
  cat234 <- reactive({
    di <- outD()
    lev <- sapply(di[sapply(di, mode) == "character"| sapply(di, class) == "factor"], table)
    lev2<- mapply(length,lev)
    leveleq2 <- lev2[lev2< 5& lev2 > 1] %>% names
    
    d_cat <- di[leveleq2]
    return(d_cat)
  })
  
  
  
  
  output$inpCategorical234 <- renderDataTable({
    d_cat<- cat234()

    ta <- cbind(
      `Variable Name`  = colnames(d_cat),
      `Levels` = apply(d_cat, 2, allLevel),
      `Missing No` =  apply(d_cat, 2,function(x) sum(is.na(x))))
    rownames(ta) <- NULL
    ta
    
  }, escape=FALSE,
  options = list(sDom  = '<"top">lrt<"bottom">ip'),selection = 'single')
  
  # cat 2, 3, 4 - the bar plot
  output$cat_bar234 <- renderPlotly({
    d<- cat234()
    if(is.null(input$inpCategorical234_rows_selected)){
      index = 1
    }else{
      index <- input$inpCategorical234_rows_selected
    }
    names <- colnames(d)[index]
    x  <-d[,index]
    p <- plot_ly(x = names(table(x)), y = table(x),type = "bar")
  })
  
  #######################
  # cat 5
  
  cat5 <- reactive({
    di <- outD()
    lev <- sapply(di[sapply(di, mode) == "character"| sapply(di, class) == "factor"], table)
    lev2<- mapply(length,lev)
    leveleq2 <- lev2[lev2 > 4] %>% names
    
    d_cat <- di[leveleq2]
    return(d_cat)
  })
  
  
  
  output$inpCategorical5 <- renderDataTable({
    d_cat<- cat5()
    ta <- cbind(
      `Variable Name`  = colnames(d_cat),
      `Levels` = apply(d_cat, 2, allLevel),
      `Missing No` =  apply(d_cat, 2,function(x) sum(is.na(x))))
    rownames(ta) <- NULL
    ta
    
  }, escape=FALSE,
  options = list(sDom  = '<"top">lrt<"bottom">ip'),
  selection = 'single')
  
  # cat 2, 3, 4 - the bar plot
  output$cat_bar5 <- renderPlotly({
    d<- cat5()
    if(is.null(input$inpCategorical5_rows_selected)){
      index = 1
    }else{
      index <- input$inpCategorical5_rows_selected
    }
    names <- colnames(d)[index]
    x  <-d[,index]
    p <- plot_ly(x = names(table(x)), y = table(x),type = "bar")
  })
  
  
  #######################
  ###5 date   ####
  date_check_data = reactiveValues(
    datarow = 2222
  )
  
  date_format <- reactive({
    do <- outD()
    di <- inpD()
    d_date_in <- cbind(`ID` = di[as.numeric(input$varID)],di[sapply(do, class) == "Date"])
    d_date_out<- cbind(`ID` = di[as.numeric(input$varID)],do[sapply(do, class) == "Date"])
    
    datalist = list()
    j = 1
    for(i in 2:length(d_date_out[1,])){
      tmp = d_date_in[is.na(d_date_out[,i]) &!is.na(d_date_in[,i])&(d_date_in[,i] != ""),]
      if(!is.null(tmp) & length(tmp[,1]> 0)){
        datalist[[j]] = tmp
        j = j + 1
      }
    }
    if(length(datalist) == 0){
      all_date = NULL
    }else if(length(datalist) == 1){
      all_date = datalist[[1]]
    }else{
      all_date = do.call(dplyr::union, datalist)
    }
    all_date
  }
  )
  
  output$wrongformDate <- renderDataTable({
    date_format()
  },
  options=list(
    sDom  = '<"top">lrt<"bottom">ip'))
  
  dateout <- reactive({
    di <- outD()
    d_date<- di[sapply(di, class) == "Date"]
    dateout <- cbind(`ID` = di[,as.numeric(input$varID)],d_date)
  }
  )
  
  observe({
    date_check_data[['table']] = dateout()
  })
  
  
  output$inpDate <- renderDataTable({
    date_check_data[['table']]
  },
  rownames = FALSE,
  selection = list(target = 'column'),
  #extensions = c('ColReorder', 'Buttons'),
  #options = list( dom = 'Bfrtip', buttons = I('colvis'),
  #                colReorder = TRUE)
  #extensions = c('Buttons'),
  #options = list( dom = 'Bfrtip', buttons = I('colvis')),
  options=list(sDom  = '<"top">lrt<"bottom">ip')
  )
  
  observeEvent(input$date_notshow,{
    di <- date_check_data[['table']]
    var_ids <- input$inpDate_columns_selected + 1
    di <- di[ , -c(var_ids)]
    date_check_data[['table']] <- di
  })
  
  observeEvent(input$date_exchage,{
    di <- date_check_data[['table']]
    var_ids <- input$inpDate_columns_selected + 1
    a <- sort(var_ids)
    b <- c(a[2],a[1])
    var_id_org <- c(1:ncol(di))
    var_id_exchange <- replace(var_id_org, a, b)
    di <- di[,var_id_exchange]
    date_check_data[['table']] <- di
  })
  
  observeEvent(input$date_reset,{
    date_check_data[['table']] <- out()
  })
  
  date_wrong <- eventReactive(input$date_check,{
    di <- date_check_data[['table']]
    n = ncol(di)
    
    for (i in 2:n){
      varname  <- paste0("col",i)
      di <-
        di %>% mutate(!!varname := 0)
    }
    
    for (i in 2:(n-1)){
      varname1  <- paste0("col",i)
      varname2  <- paste0("col",i+1)
      di[[varname1]][di[,i] > di[,i+1]] <- 1
      di[[varname2]][di[,i] > di[,i+1]] <- 1
    }
    
    t <- (di %>% select(starts_with("col")) %>% rowSums > 0 )
    do <- di[t,]
    date_check_data[['show']] <- colnames(di)[2:n]
    date_check_data[['hind']] <- di %>% select(starts_with("col")) %>% colnames()
    date_check_data[['hind_con']] <- c((n+1):length(di))
    do
  })
  
  output$date_wrong_out = renderDataTable({
    datatable(date_wrong(),
              editable = F,
              selection = 'none',
              #rownames = FALSE,
              # Hide logical columns
              options=list(
                columnDefs = list(list(targets=date_check_data[['hind_con']],
                                       visible=F)),
                sDom  = '<"top">lrt<"bottom">ip')
    ) %>%
      # Format data columns based on the values of hidden logical columns
      formatStyle(columns = date_check_data[['show']],
                  valueColumns = date_check_data[['hind']],
                  color = styleInterval(0, c('black','orange')),
                  backgroundColor = styleInterval(0, c( 'white','gray'))
      )
  }
  )
  
  
  #summary 
  
  in_table1 <- reactive({
    di=outD()
    table1out <- TheTable1(di)
    table1out
  })
  
  
  output$table1 <- renderDataTable({
    in_table1()
  }, rownames= FALSE,
  extensions = 'Scroller', options = list(
    deferRender = TRUE,
    scrollY = 400,
    scroller = TRUE)
  )
  

  #download
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
        write.csv(outD(),file, row.names = FALSE)
      }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste('Table1-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(in_table1(),file, row.names = FALSE)
    }
  )
  
  
  
}

runApp(shinyApp(ui, server), launch.browser = TRUE)

