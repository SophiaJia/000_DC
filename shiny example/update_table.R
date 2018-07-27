library(shiny)
library(DT)

shinyApp(
  ui = fluidPage(
    
    titlePanel("Sliders"),
    
    sidebarLayout(
      sidebarPanel(
        sliderInput("integer", "Integer:",
                    min=0, max=1000, value=500),
        
        sliderInput("decimal", "Decimal:",
                    min = 0, max = 1, value = 0.5, step= 0.1),
        
        sliderInput("range", "Range:",
                    min = 1, max = 1000, value = c(200,500)),
        
        sliderInput("format", "Custom Format:",
                    min = 0, max = 10000, value = 0, step = 2500,
                    pre = "$", sep = ",", animate=TRUE),
        
        sliderInput("animation", "Looping Animation:", 1, 2000, 1,
                    step = 10, animate=
                      animationOptions(interval=300, loop=TRUE,playButton = "PLAY", pauseButton = "PAUSE"))
      ),
      
      mainPanel(
        tableOutput("values"),
        DT::dataTableOutput('DTtable')
      )
    )
  ), server = function(input, output, session) {
    
    # Reactive expression to compose a data frame containing all of
    # the values
    sliderValues <- reactive({
      
      # Compose data frame
      data.frame(
        Name = c("Integer", 
                 "Decimal",
                 "Range",
                 "Custom Format",
                 "Animation"),
        Value = as.character(c(input$integer, 
                               input$decimal,
                               paste(input$range, collapse=' '),
                               input$format,
                               input$animation)), 
        stringsAsFactors=FALSE)
    }) 
    
    # Show the values using an HTML table
    output$values <- renderTable({
      sliderValues()
    })
    output$DTtable = DT::renderDataTable(rownames = FALSE, isolate({
      sliderValues()
    }))
    
    proxy = dataTableProxy('DTtable')
    observe({
      replaceData(proxy, sliderValues(), rownames = FALSE)
    })
  })