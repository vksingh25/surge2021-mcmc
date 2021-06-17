library(shiny)
library(VGAM)
library(shinyjs)

# ui
ui <- fluidPage(
  titlePanel("MH Algorithm"),
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      # buttons to select from different distributions
      selectInput("dist", "Target Distribution:",
                   c(
                     "Normal" = "norm",
                     "Pareto" = "pareto",
                     "t-Distribution" = "t.dist",
                     "Exponential" = "exp"
                    )),
      br(),
      # slider for number of draws
      sliderInput(inputId = "N",
                  label = "Number of draws",
                  min = 2,
                  max = 1e5,
                  value = 1e4),
      # slider for standard deviation
      br(),
      sliderInput(inputId = "h",
                  label = "Step Size",
                  min = 0.01,
                  max = 1,
                  value = 1,
                  step = 0.01,
                  animate = TRUE),
      animationOptions(interval = 50),
      br(),
      # slider for initial point
      sliderInput(inputId = "initial",
                  label = "Starting Point",
                  min = 0,
                  max = 100,
                  value = 3),
      br(),
      # slider for range for plot
      sliderInput("range", "Range",
                  min = -20, max = 20,
                  value = c(-5, 10)),
      # Parameters of different distributions
      br(),
      h4("Distribution Parameters"),
      fluidRow(
        column(6,
               numericInput("shape_pareto", "Pareto: Shape", value = 1, min = 0.01, max = 10),
        ),
        column(6,
               numericInput("scale_pareto", "Pareto: Scale", value = 1, min = 0.01, max = 10),
        )
      ),
      numericInput("rate_exp", "Exponential: Rate", value = 1, min = 0.01, max = 10),
      fluidRow(
        column(6,
               numericInput("mean_norm", "Normal: Mean", value = 0, min = -10, max = 10),
        ),
        column(6,
               numericInput("sd_norm", "Normal: SD", value = 1, min = 0.01, max = 10),
        )
      ),
      numericInput("df_t", "t-distribution: df", value = 1, min = 0.01, max = 10),
    ),
    mainPanel(
      # different tabs for different plots
      tabsetPanel(type = "tabs",
                  tabPanel("Density", plotOutput("density")),
                  tabPanel("ACF", plotOutput("acf")),
                  tabPanel("Trace", plotOutput("trace"))
      )
    )
  )
)


# server
server <- function(input, output){
  # hide and show shape_pareto slider based on the dist
  observeEvent(input$dist, {
    switch(input$dist,
           norm = {
             shinyjs::show("mean_norm")
             shinyjs::show("sd_norm")
             shinyjs::hide("shape_pareto")
             shinyjs::hide("scale_pareto")
             shinyjs::hide("rate_exp")
             shinyjs::hide("df_t")
           },
           exp = {
             shinyjs::hide("mean_norm")
             shinyjs::hide("sd_norm")
             shinyjs::hide("shape_pareto")
             shinyjs::hide("scale_pareto")
             shinyjs::show("rate_exp")
             shinyjs::hide("df_t")
           },
           t.dist = {
             shinyjs::hide("mean_norm")
             shinyjs::hide("sd_norm")
             shinyjs::hide("shape_pareto")
             shinyjs::hide("scale_pareto")
             shinyjs::hide("rate_exp")
             shinyjs::show("df_t")
           },
           pareto = {
             shinyjs::hide("mean_norm")
             shinyjs::hide("sd_norm")
             shinyjs::show("shape_pareto")
             shinyjs::show("scale_pareto")
             shinyjs::hide("rate_exp")
             shinyjs::hide("df_t")
           })
  })

  # target distribution (by default exp)
  target <- function(y) {
    # shape parameter for pareto dist
    shape <- input$shape_pareto
    result <- switch(input$dist,
                     exp = dexp(y, input$rate_exp),
                     norm = dnorm(y, input$mean_norm, input$sd_norm),
                     t.dist = dt(y, df = input$df_t),
                     pareto = dpareto(y, input$scale_pareto, input$shape_pareto),
                     dexp(y)
                    )
    return(result)
  }
  # function to draw values from our proposal distribution
  proposal_dist <- function(N, h, initial = 1){
    # vector of normal and uniform r.v.s (for optimization purposes)
    normals <- rnorm(N, 0, h)
    uniforms <- runif(N)
    x <- rep(0, N)
    x[1] <- initial  # initialize
    for(i in 2:N){
      current_x <- x[i-1]
      proposed_x <- current_x + normals[i]  # proposed value
      A <- min(1, target(proposed_x)/target(current_x))  # MH Acceptance rate
      ifelse(uniforms[i] < A, x[i] <- proposed_x, x[i] <- current_x)
    }
    return (x)
  }

  proposal <- reactive({
    proposal_dist(input$N, input$h, input$initial)
  })

  # For line plot of dist for comparison
  xs <- seq(-10, 10, length = 1000)
  d <- reactive({
    switch(input$dist,
           exp = dexp(xs, input$rate_exp),
           norm = dnorm(xs, input$mean_norm, input$sd_norm),
           t.dist = dt(xs, df = input$df_t),
           pareto = dpareto(xs, input$scale_pareto, input$shape_pareto),
           dexp(xs)
           )
  })
  # range for plot
  range <- reactive({
    input$range
  })

  # plots
  output$density <- renderPlot({
    plot(density(proposal()), col = "blue", xlim = range(), main = "Density plot")
    lines(xs, d(), type="l", col = 'red')
  })
  output$acf <- renderPlot({
    acf(proposal(), main = "ACF Plot")
  })
  output$trace <- renderPlot({
    plot.ts(proposal(), main = "Trace plot")
  })
}

# call to shiny app
shinyApp(ui = ui, server = server)
