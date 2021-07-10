# TODO: add start button over play pause
library(shiny)
library(shinydashboard)

chisq_den <- function(x, k) {
  if(x > 0) {
    rtn <- x^(k/2 - 1) * exp(- x/2)
  } else {
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

chisq_mh1  <- function(N = 1e3, k, h, start) {
  # memory allocation
  out <- numeric(length = N)
  acc.prob <- 0  # acceptance probability
  out[1] <- start  # just some starting value (fixed starting value)

  for(t in 2:N) {
    # proposal N(x, h). Use sd = sqrt(variance) in R
    prop <- rnorm(1, mean = 2, sd = 8^2)

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

sidebar = dashboardSidebar(
  sidebarMenu(
    menuItem("Time Evolution MH", tabName = 'evolution', icon = icon('dashboard'))
  )
)

body = dashboardBody(
  tabItems(
    tabItem(
      tabName = 'evolution',
      fluidRow(
        box(
          width = 12, solidHeader = TRUE,
          h3("Comparison between different MH Proposals as time progresses")
        ),
        column(
          width = 7,
          box(
            width = NULL, solidHeader = TRUE, status = 'primary',
            plotOutput("mh_dependent")
          ),
          box(
            width = NULL, solidHeader = TRUE, status = 'primary',
            plotOutput("mh_independent"),
          )
        ),
        column(
          width = 5,
          box(
            title = 'About Target', width = NULL, status = 'primary',
            "Chi-squared distribution with 10 degrees of freedom"
          ),
          box(
            title = "Figure 1", width = NULL, status = 'warning',
            "Metropolis Hastings algorithm using y = N(x, 0.5) as its proposal distribution where x is the current value of the Markov chain"
          ),
          box(
            title = "Figure 1", width = NULL, status = 'warning',
            "Independent Metropolis Hastings algorithm using y = N(2, 8) as its constant proposal distribution"
          ),
          box(
            title = "Parameters Box", width = NULL, status = 'primary',
            selectInput(
              "dist", "Initial Distribution: ",
              choices = c(
                "Stationary" = "stat",
                "Fixed at 3" = "fixed",
                "Exp(0.05)" = "exp1",
                "Exp(0.01)" = "exp2"
              )
            )
          ),
          box(
            title = "Control Panel", width = NULL, status = 'primary',
            fluidRow(
              column(6, uiOutput("playButton")),
              column(6, actionButton("reset", label = "Reset", width = '100%')),
            )
          )
        )
      )
    )
  )
)


ui = dashboardPage(
  dashboardHeader(title = ""),
  sidebar,
  body
)

server = function(input, output) {
  reps <- 1000
  N <- 1e2 # number of steps
  h <- .5
  k <- 10 # df of the target chi-sq distribution

  chain <- reactiveValues()
  chain$values <- matrix(0, nrow = reps, ncol = N)
  chain$values1 <- matrix(0, nrow = reps, ncol = N)
  chain$time <- 1
  chain$target <- rchisq(1e5, df = k)

  control <- reactiveValues()
  control$isRunning <- 0
  control$isStart <- 0

  output$playButton <- renderUI({
    if(control$isRunning == 0){
      actionButton("play", "Play", width = '100%')
    } else {
      actionButton("pause", "Pause", width = '100%')
    }
  })

  simulate <- function() {
    if(input$dist == "stat") {
      random.chi <- rchisq(reps, df = k)
      for(r in 1:reps) {
        # Fixed starting value
        chain$values[r, ] <- chisq_mh(N = N, k = k, h = h, start = random.chi[r])
        chain$values1[r, ] <- chisq_mh1(N = N, k = k, h = h, start = random.chi[r])
      }
    }
    else if(input$dist == "exp1") {
      random.exp1 <- rexp(reps, 0.05)
      for(r in 1:reps) {
        # Fixed starting value
        chain$values[r, ] <- chisq_mh(N = N, k = k, h = h, start = random.exp1[r])
        chain$values1[r, ] <- chisq_mh1(N = N, k = k, h = h, start = random.exp1[r])
      }
    }
    else if(input$dist == "exp2") {
      random.exp2 <- rexp(reps, 0.01)
      for(r in 1:reps) {
        # Fixed starting value
        chain$values[r, ] <- chisq_mh(N = N, k = k, h = h, start = random.exp2[r])
        chain$values1[r, ] <- chisq_mh1(N = N, k = k, h = h, start = random.exp2[r])
      }
    } else {
      for(r in 1:reps) {
        # Fixed starting value
        chain$values[r, ] <- chisq_mh(N = N, k = k, h = h, start = 3)
        chain$values1[r, ] <- chisq_mh1(N = N, k = k, h = h, start = 3)
      }
    }
  }

  forward <- function() {
    chain$time <- min(chain$time+1, N)
  }

  observeEvent(input$play,{
    control$isRunning <- 1
    # control$isStart <- 1
    if(chain$time == 1){
      simulate()
    }
    observeEvent(if(control$isRunning) {invalidateLater(1000); TRUE}, {
      forward()
    })
  })

  observeEvent(input$pause,{
    control$isRunning <- 0
  })

  ## when reset button is pressed (set everything to original values)
  observeEvent(input$reset,{
    control$isRunning <- 0
    # control$isStart <- 0
    chain$values <- matrix(0, nrow = reps, ncol = N)
    chain$values1 <- matrix(0, nrow = reps, ncol = N)
    chain$time <- 1
  })

  # main plot
  # TODO: Add description and stuff
  output$mh_dependent <- renderPlot({
    plot(density(chain$target), type = 'l', lwd = 2, main = "Density estimates", ylim = c(0,.15), xlab = paste("Number of draws: ", min(chain$time, N)))
    # Comment next 3 lines for removing red trailing lines
    if(chain$time != 1){
      for(k in 1:chain$time){
        lines(density(chain$values[, k]), col = adjustcolor("red", alpha.f = .4))
      }
      lines(density(chain$values[, chain$time]), lwd = 2, col = adjustcolor("green"))
    }
    legend("topright", col = c("black", "green", "red"), legend = c("target", "current", "prev"),
           lwd = c(2, 2, 1))
  })
  output$mh_independent <- renderPlot({
    plot(density(chain$target), type = 'l', lwd = 2, main = "Density estimates", ylim = c(0,.15), xlab = paste("Number of draws: ", min(chain$time, N)))
    # Comment next 3 lines for removing red trailing lines
    if(chain$time != 1){
      for(k in 1:chain$time){
        lines(density(chain$values1[, k]), col = adjustcolor("red", alpha.f = .4))
      }
      lines(density(chain$values1[, chain$time]), lwd = 2, col = adjustcolor("green"))
    }
    legend("topright", col = c("black", "green", "red"), legend = c("target", "current", "prev"),
           lwd = c(2, 2, 1))
  })
}

shinyApp(ui, server)
