library(shiny)

# ui
ui <- fluidPage(
  titlePanel("MH Algorithm"),
  sidebarLayout(
    sidebarPanel(
      # TODO: Add more distributions
      radioButtons("dist", "Target Distribution:",
                   c("Exponential" = "exp")),
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
      sliderInput(inputId = "initial",
                  label = "Starting Point",
                  min = 0,
                  max = 100,
                  value = 3)
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
  # target distribution (by default exp)
  #TODO: Add more distributions
  target <- function(y) {
    return (ifelse(y < 0, 0, exp(-y)))
  }
  # function to draw values from our proposal distribution
  proposal_dist <- function(N, h, initial = 3){
    # vector of normal and uniform r.v.s for optimization purposes
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

  d <- reactive({
    # TODO: add more distributions
    dist <- switch(input$dist,
                   exp = rexp)
    dist(1e5)
  })

  # plots
  output$density <- renderPlot({
    plot(density(proposal()), col = "blue", xlim = c(0,10), main = "Density plot")
    lines(density(d()), col = "red")
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
