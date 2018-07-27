library(shiny)
library(DT)

# Create DT
Col1 = c(9,5,8)
Col2 = c(9,4,7)
Col3 = c(9,9,5)
Col4 = c(8,8,7)
Argu = c(10,6,8)
df = data.frame(Col1,Col2,Col3,Col4,Argu)

# Create Shiny Output
shinyApp(
  ui =
    navbarPage("Testing",dataTableOutput('dt')),
  server = function(input, output, session) {
    output$dt = DT::renderDataTable(
      datatable(iris,
                editable = TRUE,
                selection = 'none',
                rownames = FALSE) %>%
        formatStyle(
          c('Sepal.Length','Petal.Length'),
          c('Sepal.Width','Petal.Width'),
          Color = styleInterval(0.1, c('white', 'black')),
          backgroundColor = styleInterval(0.1, c('gray', 'white'))
        )
    )}
)
