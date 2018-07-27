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
# This function will create the buttons for the datatable, they will be unique
shinyInput <- function(FUN, len, id, ...) {inputs <- character(len)
for (i in seq_len(len)) {
  inputs[i] <- as.character(FUN(paste0(id, i), ...))}
inputs
}

allLevel <- function(x){
  x  %>% table %>% names %>% paste(., collapse="; ")
}

# marker objects (con hist)
m <- list(color = toRGB("black"))
m2 <- list(color = toRGB("black", 0.2),
           line = list(color = 'black', width = 0.3))



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
                           actionButton("con_check", label = "Check"),
                           tableOutput('con_aftercheck')
                  ) ,
                  tabPanel("Categorical",
                           h4("If this isn't your first time, update the character matching criteria"),
                           hr(),
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
                           dataTableOutput("inpCategorical5"),uiOutput("popup"),
                           actionButton("refresh", label = "Refresh the table"),
                           hr(),
                           h4("Download the character matching criteria, for next use")),
                  tabPanel("Date",
                           h4("Date Summary"),
                           dataTableOutput("inpDate"),
                           h5("If one of your endpoint is time-to-event variable, pleach check below:"),
                           selectInput("startday", "Start Day Var ID", choices = c(1:27)),
                           selectInput("lastday", "Last Day Var ID", choices = c(1:27)),
                           h5("Following this the start day later than the last day or the duration is too long"),
                           textInput("eevent", "Event Variable Name", value = "Status"),
                           textInput("eevent", "Event Date", value = "date.death"),
                           h5("Following is something wrong")
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
                           h4("Exact Duplicate"),
                           dataTableOutput("dup"),
                           hr(),
                           h4("Table 1"),
                           dataTableOutput("table1"),
                           h5("We made some default changes to your data; please download for more details."))
      )
    )
  )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)

  ###0 input                                                               ####
  inpD <- reactive({
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    if (ext=="csv"){
      Org <- read.csv(input$file1$datapath,stringsAsFactors = FALSE)
    } else {
      Org <- read.excel(input$file1$datapath)
    }
    return(Org)
  }
  )

  inter_data = reactiveValues()
  outD <- reactive({
    Org=inpD()
    id  <- as.numeric(input$varID)
    Org <- Org[!Org[,id] == "",]# If ID = "" then delete this row
    #Org <- Org[!is.na(Org[,id])]
    NewDat <- FuzzyClean2(Org)
    inter_data[['afterfuzzy']] = NewDat
    return(NewDat)
  })

  ###2 con                                                               ####
  output$inpContinuous <- renderDataTable({
    di <- outD()
    d_con <- di[sapply(di, class) == "numeric" | sapply(di, class) == "integer"]

    contable <- cbind(
      #`ID` = c(1:length(colnames(d_con))),
      #`Variable name` = colnames(d_con),
      `Mean` = sapply(d_con, mean, na.rm=TRUE) %>% round(digits = 2),
      `Median` = apply(d_con, 2, median, na.rm=TRUE),
      `Minimum` = apply(d_con, 2, min, na.rm=TRUE),
      `Maximum` = apply(d_con, 2, max, na.rm=TRUE),
      `Missing No` =  apply(d_con, 2,function(x) sum(is.na(x)))
    )
    contable
  },selection = 'single')

  ## con data checking

   con_aftercheck_tmp <- eventReactive(input$con_check,{
    di <- outD()
    d_con <- di[sapply(di, class) == "numeric" | sapply(di, class) == "integer"]
    tmp <- d_con[(d_con[,input$inpContinuous_rows_selected] < as.numeric(input$con_min) | d_con[,input$inpContinuous_rows_selected] > as.numeric(input$con_max))&!is.na(d_con[,input$inpContinuous_rows_selected]) ,  ]
    out <- tmp[,c(as.numeric(input$varID),input$inpContinuous_rows_selected)]
    out
 })

  output$con_aftercheck<- renderTable(con_aftercheck_tmp())

  # histogram with bins
  # convenience function for computing xbin/ybin object given a number of bins
  compute_bins <- function(x, n) {
    list(
      start = min(x, na.rm = T),
      end = max(x, na.rm = T),
      size = (max(x, na.rm = T) - min(x, na.rm = T)) / n
    )
  }

  # the histogram
  output$con_hist <- renderPlotly({
    di <- outD()
    d_con <- di[sapply(di, class) == "numeric" | sapply(di, class) == "integer"]
    index <- input$inpContinuous_rows_selected
    if(is.null(input$inpContinuous_rows_selected)){
      index = as.numeric(input$varID)
    }

    names <- colnames(d_con)[index]
    x  <- d_con[,index]
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



  ###3 char                                                               ####

  output$inpCategorical1 <- renderDataTable({
    di <- inter_data[['afterfuzzy']]
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
    di <- inter_data[['afterfuzzy']]
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


  ## more than 5
  cat_pre <- reactive({
    di <- inter_data[['afterfuzzy']]
    lev <- sapply(di[sapply(di, mode) == "character"], table)
    lev2<- mapply(length,lev)
    levelgt5 <- lev2[ lev2 > 4] %>% names

    d_cat <- di[levelgt5]
    #d_cat <- di[sapply(di, mode) == "character"]
    d_cat

  })

  cat5re <- reactive({
    d_cat <- cat_pre()
    ta <- cbind(
      `Variable Name`  = colnames(d_cat),
      `Levels` = apply(d_cat, 2, allLevel),
      `Missing No` =  apply(d_cat, 2,function(x) sum(is.na(x))))
    rownames(ta) <- NULL
    as.data.frame(
      cbind(
        ta,
        View = shinyInput(actionButton,
                          nrow(ta),
                          'button_',
                          label = "View",
                          onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )))

  })

  output$inpCategorical5 <- renderDataTable({
    cat5re()
  },
  selection = 'single',options = list(searching = FALSE,pageLength = 10),server = FALSE, escape = FALSE,rownames= FALSE
  )

  SelectedRow <- eventReactive(input$select_button,{
    as.numeric(strsplit(input$select_button, "_")[[1]][2])
  })

  observeEvent(input$select_button, {
    toggleModal(session, "Catgt5", "open")
  })

  SelectedRowName <- eventReactive(input$select_button,{
    di <- colnames(cat_pre())
    di[SelectedRow()]
  })

  values = reactiveValues(
    datarow = 2222
  )
  values_save = reactiveValues(
    datarow = 2222
  )


  DataRow <- eventReactive(input$select_button,{
    di <- cat_pre()
    tmp <- table(di[,SelectedRow()])
    output_table <- as.data.frame(cbind(rownames(tmp), table(di[,SelectedRow()]), "UNCHANGED"))
    colnames(output_table) <- c("Type","Freq","Target Category")
    rownames(output_table) <- NULL
    output_table

  })

   observeEvent(input$select_button,{
     values[[SelectedRowName()]] = DataRow()
   })

   observeEvent(input$action, {
     updateTextInput(session,"cat_gt5_text", value="")
     mydata <- isolate(values[[SelectedRowName()]] )
     if(input$cat_gt5_text!="" && !is.null(input$datatmp_rows_selected) && input$action>0){
       mydata[,3] <- as.character(mydata[,3])
       mydata[input$datatmp_rows_selected,3] <- input$cat_gt5_text
     }
     values[[SelectedRowName()]] <- mydata
   })

   observeEvent(input$cat_pop_submit, {
     values_save[["aaa"]] <- values[[SelectedRowName()]]

   })

   observeEvent(input$refresh, {
     mydata <- isolate(inter_data[['afterfuzzy']])
     myvar  <- values_save[["aaa"]]
     myvar2 <- myvar[!grepl("UNCHANGED",myvar[,3]),]

     for (i in 1:nrow(myvar2)){
           mydata[mydata[,SelectedRowName()] == myvar2[i,1] & !is.na(mydata[,SelectedRowName()]),SelectedRowName()] <- myvar2[i,3]
       }
     mydata[,SelectedRowName()] = fixchar_col(mydata[,SelectedRowName()])
     inter_data[['afterfuzzy']] <- mydata
   })


  output$datatmp <- renderDataTable(values[[SelectedRowName()]],
                                        extensions = 'Scroller', options = list(deferRender = TRUE,scrollY = 400,scroller = TRUE)
  )

  output$popup <- renderUI({
    bsModal("Catgt5",
            paste0("Variable Names: ",SelectedRowName()), "", size = "large",
            column(12,
                   dataTableOutput("datatmp"),
                   div(style = "position:absolute;right:1em;",
                       actionButton('select_all',  'Select All'),
                       actionButton('select_none', 'Unselect')),
                   textInput("cat_gt5_text", label = h4("Target Categories"), value = " "),
                   actionButton("action", label = "Update"),
                   div(style = "position:absolute;right:5em;",
                       actionButton('cat_pop_submit',  'Save'))
                   )

    )
  })

  proxy = dataTableProxy('datatmp')
  observeEvent(input$select_none, {
    proxy %>% selectRows(NULL)
  })
  observeEvent(input$select_all, {
    proxy %>% selectRows(input$datatmp_rows_current)
  })

  # refresh the table  (butten; cat_refresh)
  ## update the table
  ## clean up Org
  ## ready for download



  ###4 Date                                                              ####

  output$inpDate <- renderDataTable({
    di <- outD()
    d_date<- di[sapply(di, class) == "Date"]
    datetable <- cbind(
      #`ID` = c(1:length(colnames(d_date))),
      #`Variable Name` = colnames(d_date),
      `First Day` = apply(d_date, 2, min, na.rm=TRUE),
      `Last Day` = apply(d_date, 2, max, na.rm=TRUE),
      `Missing No` =  apply(d_date, 2,function(x) sum(is.na(x)))
    )
    datetable
  })

  output$afterclean <- renderDataTable({
    di <- outD()
    di
  },
  selection = list(target = 'column'),
  rownames= FALSE)

  output$wide <- renderDataTable({
    di <- outD()
    var_id <- input$afterclean_columns_selected + 1
    id_name  <- colnames(di)[as.numeric(input$varID)]
    Dwild <- long2wide(di, ID = id_name , var_base = di[,var_id] %>%  colnames())
    Dwild
  },
  rownames= FALSE)



  output$inpSummary <- renderText({
    d1 <- inpD()
    rbind(c("Number of Patient:", length(d1[,1])))

  })


  output$table1 <- renderDataTable({
    di=inter_data[['afterfuzzy']]
    table1out <- TheTable1(di)
    table1out
  }, rownames= FALSE,
  extensions = 'Scroller', options = list(
    deferRender = TRUE,
    scrollY = 400,
    scroller = TRUE)
  )

  output$downloadData <- downloadHandler(
    filename = function() {
      "processed.csv"
    },
    content = function(file) {
      write.csv(inter_data[['afterfuzzy']],file, row.names = FALSE)
    }
  )


}

shinyApp(ui, server)
#killing this via the console STOP sign may be better than via the web interace (if the latter crashes R)
#runApp('Sophia_0531_shiny.R',display.mode = "Normal")
