library(shiny)

# ui
ui <- fluidPage(
  titlePanel("Addition of two numbers"),
  # Sidebar layout with input and output definitions
  sidebarLayout(
    position="right",  # sidebar on the right side of the page
    # Sidebar panel to input two numbers for addition
    sidebarPanel(
      numericInput(
        inputId = "value1",
        label = "First Value",
        value = 0,
        min = -1e5,
        max = 1e5
      ),
      numericInput(
        inputId = "value2",
        label = "Second Value",
        value = 0,
        min = -1e5,
        max = 1e5
      )
    ),
    # main panel displaying addtion of the inputted numbers
    mainPanel(
      textOutput("add"),
      # styles added for the output message
      tags$head(tags$style("#add{
                             font-size: 25px;
                             font-weight: bold;
                            }"
        )
      )
    )
  ),
)

# server logic
server <- function(input, output) {
  # reactive expression for vector storing the input values
  values <- reactive({
    c(input$value1, input$value2)
  })
  # Final output displayed
  output$add <- renderText({
    paste(values()[1], " + ", values()[2], " = ", sum(values()))
  })

}

# call to shinyApp
shinyApp(ui = ui, server = server)


