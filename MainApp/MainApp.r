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
  fluidPage(
    box(
      title = 'About MCMC Algorithm', width = NULL, status = 'primary',
      # Everything about selected algorithm
      textOutput("algoDesc")
    ),
    fluidRow(
      column(
        width = 9,
        box(
          title = "MH Plot", width = NULL,
          plotOutput("mh_density")
        )
      ),
      column(
        width = 3,
        box(
          title = "About Target", width = NULL,
          textOutput("aboutTarget")
        ),
        box(
          title = "Slider", width = NULL,
          sliderInput(inputId = "targetAnimation", label = "Animation", min = 1, max = 10, value = 10)
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "ACF Plot", width = NULL,
          "What are ACF Plots, how to read them, etc",
          plotOutput("mh_acf")
        )
      ),
      column(
        width = 6,
        box(
          title = "Trace Plot", width = NULL,
          "What are Trace Plots, how to read them, etc",
          plotOutput("mh_trace")
        )
      )
    ),
    box(
      title = 'About Time Evolution', width = NULL, status = 'primary',
      # Everything about selected algorithm
      textOutput("timeDesc")
    ),
    fluidRow(
      column(
        width = 9,
        tabBox(
          title = "Figure 1", id = "tabset1", width = NULL,
          tabPanel(title = "Animation", plotOutput("mh_anime"), value = 1),
          tabPanel(title = "Static", plotOutput("mh_static"), value = 2)
        ),
      ),
      column(
        width = 3,
        box(
          title = "Slider", width = NULL,
          sliderInput(inputId = "time", label = "Number of Draws", min = 0, max = 100, value = 0)
        )
      )
    ),
  )
)

ui = dashboardPage(
  dashboardHeader(titleWidth = 250, title = ""),
  sidebar,
  body
)

server = function(input, output) {

}

shinyApp(ui, server)

