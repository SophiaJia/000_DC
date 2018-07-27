library(shiny)
library(shinyBS)

s = shinyServer(function(input,output,session){
  output$text = renderText({
    print(input$last_modal_close)
    sprintf('modal closed at %s',input$last_modal_close)
  })
})

u = shinyUI(fluidPage(
  tags$script('
              $( document ).ready(function() {
              $("#myModal").on("hidden.bs.modal", function (event) {
              x = new Date().toLocaleString();
              window.alert("Ok modal was closed at " + x);
              Shiny.onInputChange("last_modal_close",x);
              });
              })
              '),
  textOutput('text'),

  bsModal('myModal','Hi','trig','This is a modal'),
  a(href='#',onclick="$('#myModal').modal('show');",'click to open the model')
  ))

shinyApp(u,s)