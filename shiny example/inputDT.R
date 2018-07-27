library(shiny)
library(DT)
library(dplyr)


shinyApp(
  ui <- fluidPage(DT::dataTableOutput("ruless"),
                  fluidRow(column(6, offset = 2, actionButton("save", "Done", width = 200))),
                  textOutput("whole")),
  
  server <- function(input, output) {
    
    values <- reactiveValues(data = NULL)
    
    values$data <- as.data.frame(
      cbind(`variable` =c("a", "d", "b", "c", "e", "f"),
            `max` =c(1463, 159, 54, 52, 52, 220),
            `min` = c(0.7315, 0.0795, 0.027, 0.026, 0.026, 0.11)
      )
    )
    
    shinyInput = function(FUN, len, id, ...) {
      #validate(need(character(len)>0,message=paste("")))
      inputs = character(len)
      for (i in seq_len(len)) {
        inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
      }
      inputs
    }
    
    output$ruless <- DT::renderDataTable({
      datatable(
        data.frame(values$data,
                   upper.range=shinyInput(textInput,nrow(values$data),"cbox_"), 
                   lower.range=shinyInput(textInput,nrow(values$data),"cbox_")),
        selection="multiple",
        escape = FALSE,
        filter = list(position = 'top', clear = FALSE),
        extensions = list("ColReorder" = NULL, "Buttons" = NULL),
        options = list(
          dom = 'BRrltpi',
          autoWidth=TRUE,
          lengthMenu = list(c(10, 50, -1), c('10', '50', 'All')),
          ColReorder = TRUE,
          preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
          drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '),
          buttons = list(
            'copy',
            'print',
            list(
              extend = 'collection',
              buttons = c('csv', 'excel', 'pdf'),
              text = 'Download',
              selected = TRUE
            )
          )
        )
      )
    })
    output$whole <-renderText(
      {
        d<- input$save 
        d
          }
    )
    
    
    
    
    
    
    
  }
)

