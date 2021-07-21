# Stuff left to do!
"if(FALSE){
  0. TODOs
    0.1 Merge Start and Reset button
  1. Basic MH demo
    1.1 Select Target (dropdown) : Add Normal(mean, sd)(default), t-dist(df), chi-sq(df) dist as options
    1.2 Select kernel (dropdown) : (Guassian MH, independent MH)
    1.2 Select step size (numeric input) for MH
    1.3 Description of how MH works
    1.4 MH Plot (add red green dots later)
    1.5 Description of ACF and TS
    1.6 ACF and TF in the row
  2. Animation applet
  3. Law of Large Numbers
  4. CLT
  5. Credits and stuff
}"

library(shiny)
library(shinydashboard)
library(ggplot2)

sidebar = dashboardSidebar(
  width = 250,
  selectInput(
    "dist", "Target Distribution",
    c(
      "Chi-squared"="chisq"
    )
  ),
  numericInput("df_chisq", "Chi-squared : df", value = 10, min = 1, max = 20, step = 1),
  selectInput(
    "kernel", "Update Mechanism",
    c(
      "Guassian Metropolis Hastings"="mh_dep",
      "Independent MH"="mh_indep"
    )
  ),
  numericInput("h", "Step Size", value = 100, min = 0.05, max = 1000),
  actionButton(inputId = 'start', label = 'Start', width = 220),
  actionButton(inputId = 'reset', label = 'Reset', width = 220)
)

body = dashboardBody(

)

ui = dashboardPage(
  dashboardHeader(titleWidth = 250, title = ""),
  sidebar,
  body
)

server = function(input, output) {

}

shinyApp(ui, server)

