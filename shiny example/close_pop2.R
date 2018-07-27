library(shiny)
shinyApp(
  ui <- fluidPage(
    fluidRow(
      ## Button to display modal:
      actionButton(inputId = "display_modal",label = "Display modal"),
      ## Print the choices that were made in the modal:
      h1(textOutput("checked_letters"))
    )
  ),

  server <- function(input, output) {
    ## These values allow the actions made in the modal to be delayed until the
    #  modal is closed
    values = reactiveValues(to_print = "",   ## This is the text that will be displayed
                            modal_closed=F)  ## This prevents the values$to_print output from
    #  updating until the modal is closed

    ## Open the modal when button clicked
    observeEvent(input$display_modal,{
      values$modal_closed <- F
      showModal(modalDialog(
        checkboxGroupInput("checkboxes",label = "Select letters",
                           choices = LETTERS[1:7]),
        ## This footer replaces the default "Dismiss" button,
        #  which is 'footer = modalButton("Dismiss")'
        footer = actionButton("dismiss_modal",label = "Dismiss")
      ))
    })

    ## This event is triggered by the actionButton inside the modalDialog
    #  It closes the modal, and by setting values$modal_closed <- T, it
    #  triggers values$to_print to update.
    observeEvent(input$dismiss_modal,{
      values$modal_closed <- T
      removeModal()
    })
    ## values$to_print is only updated once the modal is closed.
    observe({
      if(values$modal_closed){
        values$to_print <- paste(input$checkboxes)
      }
    })
    ## Forward the values$to_print to the UI
    output$checked_letters = renderText({values$to_print})
  }
)
