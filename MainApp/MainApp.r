
library(shiny)
library(shinydashboard)
library(ggplot2)

sidebar = dashboardSidebar(

)

body = dashboardBody(

)

ui = dashboardPage(
  dashboardHeader(title = ""),
  sidebar,
  body
)

server = function(input, output) {

}

shinyApp(ui, server)

