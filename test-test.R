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
                           br(),
                           br(),
                           br(),
                           dataTableOutput('con_aftercheck'),
                           actionButton("refresh_con", label = "Refresh")
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
                           h4("Reorder the date column"),
                           dataTableOutput("inpDate"),
                           actionButton("date_exchage", label = "Reorder"),
                           actionButton("date_notshow", label = "Hide"),
                           actionButton("date_reset", label = "Reset"),
                           actionButton("date_check",   label = "Check"),
                           #h5("If one of your endpoint is time-to-event variable, pleach check below:"),
                           #selectInput("startday", "Start Day Var ID", choices = c(1:27)),
                           #selectInput("lastday", "Last Day Var ID", choices = c(1:27)),
                           #h5("Following this the start day later than the last day or the duration is too long"),
                           #textInput("eevent", "Event Variable Name", value = "Status"),
                           #textInput("eevent", "Event Date", value = "date.death"),
                           h4("Something wrong with the date"),
                           dataTableOutput("date_wrong_out"),
                           actionButton("refresh_date", label = "Refresh")

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
