# Stuff left to do!
"if(FALSE){
  0. TODOs
    0.1 Merge Start and Reset button
    0.3 Include range slider for plot
    0.2 Return acceptance Probability
    0.3 Starting distribution dropdown addition
    0.n Use C++ for loops
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
  ###########################################
  # general functions required throughout the app
  ###########################################
  reps = 1e3
  N = 1e2
  colors_red_static = rainbow(n=N/2, start = 1/25, end = 1/8, alpha=0.2)
  colors_blue_static = rainbow(n=N/2, start = 1/1.85, end = 1/1.65, alpha = 0.2)
  colors_static = c(colors_red_static, colors_blue_static)
  colors_red_anime = rainbow(n=N/2, start = 1/25, end = 1/8, alpha=1)
  colors_blue_anime = rainbow(n=N/2, start = 1/1.85, end = 1/1.65, alpha = 1)
  colors_anime = c(colors_red_anime, colors_blue_anime)
  # TODO: make it reactive
  target = rchisq(1e5, df = 10)

  controls = reactiveValues()
  controls$computed = 0

  # returns target density
  target_den = function(x){
    rtn = 0
    if(input$dist == 'chisq') {
      k = input$df_chisq
      if(x > 0) {
        rtn = x^(k/2 - 1) * exp(- x/2)
      } else {
        rtn = 0
      }
    }
    return (rtn)
  }

  # returns proposal values using selected algorithm
  target_mh = function(N, start = 3){
    out = numeric(length = N)
    acc.prob = 0  # acceptance prob
    out[1] = start
    mean = 2
    for(t in 2:N) {
      if(input$kernel == 'mh_dep'){
        mean = out[t-1]
      }
      # proposal N(x, h). Use sd = sqrt(variance) in R
      prop = rnorm(1, mean = mean, sd = sqrt(input$h))

      # the proposal density gets cancelled here
      alpha = target_den(x = prop) / target_den(x = out[t-1])

      U = runif(1)
      if(U <= alpha)  # to decide whether to accept or reject
      {
        out[t] = prop
        acc.prob = acc.prob + 1
      } else {
        out[t] = out[t-1]
      }
    }
    # print(acc.prob/N)   # we want to see often we accept
    return(out)
  }

  #######################################
  # functions and variables required for app 1
  #######################################
  # density = reactiveValues()
  # density$proposal = target_mh(N, 3)
  proposal = reactive({
    target_mh(N = 1e3, start = 3)
  })
  # output plots of app 1
  output$mh_density = renderPlot({
    ggplot(data = data.frame(output = proposal()), mapping = aes(x = output, color = 'blue', linetype = 'current')) +
      geom_line(stat = 'density') +
      geom_line(data = data.frame(target = target), mapping = aes(x = target, color = 'black', linetype = 'target'), stat = 'density', lty = 2) +
      scale_color_manual(name = 'Legend', values = c('blue' = 'blue', 'black' = 'black'), labels = c('current', 'target')) +
      scale_linetype_manual(name = 'Legend', values = c('current' = 1, 'target' = 2)) +
      ylab('Density') + xlab(N) +
      labs(title = 'Density Plot') +
      theme_classic()
  })
  output$mh_acf <- renderPlot({
    acf(proposal(), main = "ACF Plot")
  })
  output$mh_trace <- renderPlot({
    ggplot(data = data.frame(output = proposal(), y = 1:1e3), mapping = aes(x = y, y = output)) +
      geom_line() +
      xlab("Time") + ylab("Proposal") +
      labs(title = "Trace Plot")
  })

}

shinyApp(ui, server)

