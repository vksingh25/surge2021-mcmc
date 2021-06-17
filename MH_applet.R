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
      radioButtons("dist", "Target Distribution:",
                   c(
                     "Normal" = "norm",
                     "Pareto" = "pareto",
                     "Cauchy" = "cauchy",
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
                  label = "standard deviation",
                  min = 0.01,
                  max = 1,
                  value = 1,
                  step = 0.01,
                  animate = TRUE),
      animationOptions(interval = 5),
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
      # Shape parameter for pareto distribution
      br(), br(),
      sliderInput("shape_pareto", "Pareto: Shape",
                  min = 1, max = 10,
                  value = 1),
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
    if(input$dist == "pareto"){
      shinyjs::show("shape_pareto")
    } else {
      shinyjs::hide("shape_pareto")
    }
  })


  # target distribution (by default exp)
  target <- function(y) {
    # shape parameter for pareto dist
    shape <- input$shape_pareto
    result <- switch(input$dist,
                     exp = ifelse(y >= 0, exp(-y), 0),
                     norm = exp(-(y^2)/2),
                     cauchy = 1/(1+y^2),
                     pareto = ifelse(y >= 1, shape/(y^(1+shape)), 0),
                     ifelse(y >= 0, exp(-y), 0)
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
      if(uniforms[i] < A) {
        x[i] <- proposed_x
      } else {
        x[i] <- current_x
      }
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
           exp = dexp(xs),
           norm = dnorm(xs),
           cauchy = dcauchy(xs),
           pareto = dpareto(xs, shape = input$shape_pareto),
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

