
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("MH Time evolution"),
  sidebarLayout(
    sidebarPanel(
      # numericInput("sd", "Standard Deviation", 1, min = 0, max = 10),
      # hr(style="border-color: grey;"),
      # start reset buttons
      fluidRow(
        column(7, uiOutput("resetButton")),
        column(5, uiOutput("startButton"))
      ),
      hr(style="border-color: grey;"),
      fluidRow(
        column(7, actionButton("stop","Stop")),
        column(3, actionButton("play","Play"))
      ),
    ),
    mainPanel(
      plotOutput("density"),
    ),
  ),
)

server <- function(input, output) {

  reps <- 1000
  N <- 1e2 # number of steps
  h <- .5
  k <- 10 #df of the target chi-sq distribution
  chain.fixed <- matrix(0, nrow = reps, ncol = N)

  waits <- reactiveValues()
  waits$resetIndicator <- 0
  waits$data <- matrix(0, nrow = reps, ncol = N)
  waits$i <- 1
  waits$target <- rchisq(1e6, df = k)



  output$resetButton <- renderUI({
    if(waits$resetIndicator == 0){
      lbl <- "Set Parameters"
    } else {
      lbl <- "Reset"
    }
    actionButton("reset", lbl)
  })

  chisq_den <- function(x, k) {
    if(x > 0)
    {
      rtn <- x^(k/2 - 1) * exp(- x/2)
    } else{
      rtn <- 0
    }
    return(rtn)
  }

  chisq_mh  <- function(N = 1e3, k, h, start) {
    # memory allocation
    out <- numeric(length = N)
    acc.prob <- 0  # acceptance probability
    out[1] <- start  # just some starting value (fixed starting value)

    for(t in 2:N) {
      # proposal N(x, h). Use sd = sqrt(variance) in R
      prop <- rnorm(1, mean = out[t-1], sd = sqrt(h))

      # the proposal density gets cancelled here
      alpha <- chisq_den(x = prop, k = k) / chisq_den(x = out[t-1], k = k)

      U <- runif(1)
      if(U <= alpha)  # to decide whether to accept or reject
      {
        out[t] <- prop
        acc.prob <- acc.prob + 1
      } else {
        out[t] <- out[t-1]
      }
    }
    print(acc.prob/N)   # we want to see often we accept
    return(out)
  }

  for(r in 1:reps) {
    # Fixed starting value
    chain.fixed[r, ] <- chisq_mh(N = N, k = k, h = h, start = 3)
  }

  forward <- function() {
    waits$resetIndicator <- 1 # change button label
    waits$data <- chain.fixed
    waits$i <- min(waits$i+1, N)
  }


  session <- reactiveValues()
  session$timer <- reactiveTimer(Inf)

  observeEvent(input$play,{
    session$timer <- reactiveTimer(250)
    observeEvent(session$timer(),{
      forward()
    })
  })

  observeEvent(input$stop,{
    session$timer <- reactiveTimer(Inf)
  })

  ## when reset button is pressed (set everything to original values)
  observeEvent(input$reset,{
    waits$resetIndicator <- 0
    waits$data <- matrix(0, nrow = reps, ncol = N)
    waits$i <- 1
  })

  # main plot
  output$density <- renderPlot({
    plot(density(waits$target), type = 'l', lwd = 2, main = "Density estimates from fixed", ylim = c(0,.15))
    lines(density(waits$data[, waits$i]), lwd = 5, col = adjustcolor("green"))
    # Comment 127-129 for removing red trailling lines
    for(k in 1:waits$i){
      lines(density(waits$data[, k]), col = adjustcolor("red", alpha.f = .3))
    }
    # ggplot(data = data.frame(output = waits$data[, waits$i]), mapping = aes(x = output, color = 'red')) +
    #   geom_line(stat = 'density') +
    #   geom_line(data = data.frame(target = waits$target), mapping = aes(x = target, color = 'black'), stat = 'density') +
    #   labs(title = "Density estimates from fixed")
  })





}

shinyApp(ui, server)
