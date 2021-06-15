library(shiny)

# ui
ui <- fluidPage(
  titlePanel("MH Algorithm")
  sidebarLayout(
    position = "right",
    sidebarPanel(
      # Dropdown for distributions, by default exponential
      # Slider for number of inputs
      # Slider for standard deviation
    ),
    mainPanel(
      # Comparison plot
      # acf plot
      # trace plot
    ),
  )
)

# server
server <- function(input, output){

}

# call to shiny app
shinyApp(ui = ui, server = server)
