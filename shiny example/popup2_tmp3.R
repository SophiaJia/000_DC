#rm(list = ls())
library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinydashboard)

# This function will create the buttons for the datatable, they will be unique
shinyInput <- function(FUN, len, id, ...) {inputs <- character(len)
for (i in seq_len(len)) {
  inputs[i] <- as.character(FUN(paste0(id, i), ...))}
inputs
}

ui <- dashboardPage(
  dashboardHeader(title = "Simple App"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Menu Item 1", tabName = "one", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "one",h2("Datatable Modal Popup"),
              DT::dataTableOutput('my_table'),uiOutput("popup")
      )
    )
  )
)

server <- function(input, output, session) {
  my_data <- reactive({
    testdata <- cars
    as.data.frame(
      cbind(
      View = shinyInput(actionButton, 
                        nrow(testdata),
                        'button_', 
                        label = "View", 
                        onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
      testdata))
  })  
  output$my_table <- DT::renderDataTable(my_data(),selection = 'single',options = list(searching = FALSE,pageLength = 10),server = FALSE, escape = FALSE,rownames= FALSE)
  
  SelectedRow <- eventReactive(input$select_button,{
    as.numeric(strsplit(input$select_button, "_")[[1]][2])
  })
  observeEvent(input$select_button, {
    toggleModal(session, "modalExample", "open")
  })
  
  DataRow <- eventReactive(input$select_button,{
    iris
  })
  
  ## I guess my input name is not right
  output$y11 = renderPrint(input$modalExample_rows_selected)
  
  
  output$popup <- renderUI({
    bsModal("modalExample", paste0("Data for Row Number: ",SelectedRow()), "", size = "large",
            column(12,                   
                   DT::renderDataTable(DataRow()),
                   h4("The following didn't show when I select the rows"),
                   verbatimTextOutput('y11')
                   
            )
    )
  })
  
}
shinyApp(ui, server)