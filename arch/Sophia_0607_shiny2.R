library(shiny)
library(tidyverse)
library(openxlsx)
library(janitor)
library(stringdist)
library(lubridate)
library(tableone)
library(shinyBS)
library(shinyjs)

#library(scRub)

# This function will create the buttons for the datatable, they will be unique
shinyInput <- function(FUN, len, id, ...) {inputs <- character(len)
for (i in seq_len(len)) {
  inputs[i] <- as.character(FUN(paste0(id, i), ...))}
inputs
}

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
                           DT::dataTableOutput("inpContinuous"),
                           h4("Data Checking")#,
                            # selectInput("concheck", "Variable ID", choices = c(1:27)),
                            # textInput("conmin", "Minimum value", value = "1"),
                            # textInput("commax", "Maximum value", value = "100"),
                           #DT::dataTableOutput("feedContinuous")
                            #actionButton('insertBtn', 'Insert'),
                            #actionButton('removeBtn', 'Remove'),
                            #tags$div(id = 'placeholder')
                  ) ,
                  tabPanel("Categorical",
                           h4("1.Variables with one level, recommend remove"),
                           h5("These variables only have one layer. It might because 1) all of the patients have the same value, in which case you can remove this column; 2) it is non-random missing, in which case you should check your data."),
                           DT::dataTableOutput("inpCategorical1"),
                           hr(),
                           h4("2.Variables with 2-4 levels, check correctness"),
                           h5("These variables have 2 - 4 levels, which is suitable for analysis. However, you still need to check if there are typos and make sure that the level number is correct. E.g., gender should has only two levels. "),
                           DT::dataTableOutput("inpCategorical234"),
                           hr(),
                           h4("3.Variables more than 5 levels, recommend clean up"),
                           h5("These variables have more than four levels. It usually is not suitable for analysis. We recommend you to check if to and try your best to compress less than four. "),
                           DT::dataTableOutput("inpCategorical5"),uiOutput("popup")),
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
                  tabPanel("Cleaned Data",
                           h5("If your data is repeated measure and has per visit per row,choose the column that indicates patient characteristics"),
                           DT::dataTableOutput("afterclean")
                           ),
                  tabPanel("Wide Type Data",
                           h5("This is for repeated measure data only"),
                           DT::dataTableOutput("wide")),
                  tabPanel("Summary",
                           h4("Summary"),
                           textOutput("inpSummary"),
                           hr(),
                           h4("Exact Duplicate"),
                           DT::dataTableOutput("dup"),
                           hr(),
                           h4("Table 1"),
                           DT::dataTableOutput("table1"),
                           h5("We made some default changes to your data; please download for more details."))
      )
    )
  )
)

server <- function(input, output, session) {
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


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### tag3 char                                                               ####

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

  }, escape=FALSE,
  options = list(sDom  = '<"top">lrt<"bottom">ip'))


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

  }, escape=FALSE,
  options = list(sDom  = '<"top">lrt<"bottom">ip'))

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

  }, escape=FALSE,
  options = list(sDom  = '<"top">lrt<"bottom">ip'))


  ## more than 5
  cat_pre <- reactive({
    di <- outD()
    lev <- sapply(di[sapply(di, mode) == "character"], table)
    lev2<- mapply(length,lev)
    levelgt5 <- lev2[ lev2 > 4] %>% names

    d_cat <- di[levelgt5]
    d_cat

  })

  cat5 <- reactive({

    d_cat <- cat_pre()
    allLevel <- function(x){
      x  %>% table %>% names %>% paste(., collapse="; ")
    }
    ta <- cbind(
      `Variable Name`  = colnames(d_cat),
      `Levels` = apply(d_cat, 2, allLevel))
    rownames(ta) <- NULL
    ta})

  cat5re <- reactive({
    ta = cat5()
    as.data.frame(
      cbind(
        ta,
        View = shinyInput(actionButton,
                          nrow(ta),
                          'button_',
                          label = "View",
                          onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )))
  })

  output$inpCategorical5 <- DT::renderDataTable({
    cat5re()
  },
  selection = 'single',options = list(searching = FALSE,pageLength = 10),server = FALSE, escape = FALSE,rownames= FALSE
  )

  # Here I created a reactive to save which row was clicked which can be stored for further analysis
  SelectedRow <- eventReactive(input$select_button,{
    as.numeric(strsplit(input$select_button, "_")[[1]][2])
  })

  # This is needed so that the button is clicked once for modal to show, a bug reported here
  observeEvent(input$select_button, {
    toggleModal(session, "Cat_gt5", "open")
  })

  SelectedRowName <- eventReactive(input$select_button,{
    di <- colnames(cat_pre())
    di[SelectedRow()]
  })

  orgdata_gt5 <- eventReactive(input$select_button,{
    di <- cat_pre()
    tmp <- table(di[,SelectedRow()])
    output_table <- as.data.frame(cbind(rownames(tmp), table(di[,SelectedRow()]), "Unknown"))
    colnames(output_table) <- c("Type","Freq","Target Category")
    rownames(output_table) <- NULL
    output_table
  })

  intdata <- reactive({orgdata_gt5()})


  ntext <- eventReactive(input$action, {
    input$cat_gt5_text
  })

  showdata <- eventReactive(input$action,{
    di <- intdata()
    if(input$cat_gt5_text!="" &&input$action>0){
      di[input$popup_rows_selected,3] = ntext()
    }
    di[input$popup_rows_selected, ]
  }, ignoreNULL = FALSE)



  output$popup <- renderUI({
    bsModal("Cat_gt5",
            paste0("Variable Names: ",SelectedRowName()), "", size = "large",
            column(12,
                   DT::renderDataTable(intdata(),
                                       rownames= FALSE
                                      ),
                   textInput("cat_gt5_text", label = h4("Target Categories"), value = " "),
                   actionButton("action", label = "Update"),
                   hr(),
                   DT::renderDataTable(showdata(),
                                       rownames= FALSE
                   ))
           )
  })


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### tag 4 Date                                                              ####


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

  output$afterclean <- DT::renderDataTable({
    di <- outD()
    di
  },
  selection = list(target = 'column'),
  rownames= FALSE)

  output$wide <- DT::renderDataTable({
    di <- outD()
    var_id <- input$afterclean_columns_selected + 1
    id_name  <- colnames(di)[as.numeric(input$varID)]
    Dwild <- long2wide(di, ID = id_name , var_base = di[,var_id] %>%  colnames())
    Dwild
  },
  rownames= FALSE)

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
      "processed.csv"
    },
    content = function(file) {
      write.csv(Dwild(),file, row.names = FALSE)
    }
  )


}

shinyApp(ui, server)
#killing this via the console STOP sign may be better than via the web interace (if the latter crashes R)
#runApp('Sophia_0531_shiny.R',display.mode = "Normal")
