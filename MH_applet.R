library(shiny)
library(VGAM)
library(shinyjs)
library(ggplot2)

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
                  max = 1e4,
                  value = 100),
      # slider for standard deviation
      br(),
      # numeric input for step size
      numericInput("h", "Step Size", min = 0.01, max = 10, value = 1),
      br(),
      # slider for initial point
      sliderInput(inputId = "initial",
                  label = "Starting Point",
                  min = 0,
                  max = 100,
                  value = 3),
      br(),
      # Parameters of different distributions
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
  proposal_dist <- function(N = 100, h = 1, initial = 1){
    # vector of normal and uniform r.v.s (for optimization purposes)
    normals <- rnorm(N, 0, h)
    uniforms <- runif(N)
    x <- rep(0, N)
    acceptance <- rep(0, N)
    x[1] <- initial  # initialize
    acceptance[1] <- 1
    for(i in 2:N){
      current_x <- x[i-1]
      proposed_x <- current_x + normals[i]  # proposed value
      A <- min(1, target(proposed_x)/target(current_x))  # MH Acceptance rate
      if(uniforms[i] < A){
        x[i] <- proposed_x
        acceptance[i] <- 1
      } else {
        x[i] <- current_x
      }
    }
    return (list(x, acceptance))
  }

  N <- reactive ({input$N})

  proposal <- reactive({
    proposal_dist(input$N, input$h, input$initial)
  })

  # For line plot of dist for comparison
  d <- reactive({
    switch(input$dist,
           exp = rexp(1e4, input$rate_exp),
           norm = rnorm(1e4, input$mean_norm, input$sd_norm),
           t.dist = rt(1e4, df = input$df_t),
           pareto = rpareto(1e4, input$scale_pareto, input$shape_pareto),
           dexp(1e4)
           )
  })

  acceptance <- reactive({
    rate <- mean(unlist(proposal()[2]))
    format(round(rate, 2), nsmall = 2)
  })

  # plots
  output$density <- renderPlot({
    ggplot(data = data.frame(output = unlist(proposal()[1])), mapping = aes(x = output, color = 'blue', linetype = 'current')) +
      geom_line(stat = 'density') +
      geom_line(data = data.frame(target = d()), mapping = aes(x = target, color = 'red', linetype = 'target'), stat = 'density', lty = 2) +
      scale_color_manual(name = 'Legend', values = c('blue' = 'blue', 'red' = 'red'), labels = c('current', 'target')) +
      scale_linetype_manual(name = 'Legend', values = c('current' = 1, 'target' = 2)) +
      ylab('Density') + xlab(paste("N = ", N(), ", Acceptance rate = ", acceptance())) +
      labs(title = "Density Plot")
  })
  output$acf <- renderPlot({
    acf(unlist(proposal()[1]), main = "ACF Plot")
  })
  output$trace <- renderPlot({
    ggplot(data = data.frame(output = unlist(proposal()[1]), y = 1:N()), mapping = aes(x = y, y = output)) +
      geom_line() +
      xlab("Time") + ylab("Proposal") +
      labs(title = "Trace Plot")
  })
}

# call to shiny app
shinyApp(ui = ui, server = server)
