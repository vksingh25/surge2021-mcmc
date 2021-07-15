# TODO: color gradient scheme
# TODO: disable reset button + numeric inputs when paused (using shinyjs)
# TODO: make static graph actually static

library(shiny)
library(shinydashboard)

chisq_den = function(x, k) {
  if(x > 0) {
    rtn = x^(k/2 - 1) * exp(- x/2)
  } else {
    rtn = 0
  }
  return(rtn)
}

chisq_mh_dep  = function(N = 1e3, k = 10, h = 0.5, start) {
  # memory allocation
  out = numeric(length = N)
  acc.prob = 0  # acceptance probability
  out[1] = start  # just some starting value (fixed starting value)

  for(t in 2:N) {
    # proposal N(x, h). Use sd = sqrt(variance) in R
    prop = rnorm(1, mean = out[t-1], sd = sqrt(h))

    # the proposal density gets cancelled here
    alpha = chisq_den(x = prop, k = k) / chisq_den(x = out[t-1], k = k)

    U = runif(1)
    if(U <= alpha)  # to decide whether to accept or reject
    {
      out[t] = prop
      acc.prob = acc.prob + 1
    } else {
      out[t] = out[t-1]
    }
  }
  print(acc.prob/N)   # we want to see often we accept
  return(out)
}

chisq_mh_indep  = function(N = 1e3, k = 10, h = 8^4, start) {
  # memory allocation
  out = numeric(length = N)
  acc.prob = 0  # acceptance probability
  out[1] = start  # just some starting value (fixed starting value)

  for(t in 2:N) {
    # proposal N(x, h). Use sd = sqrt(variance) in R
    prop = rnorm(1, mean = 2, sd = sqrt(h))

    # the proposal density gets cancelled here
    alpha = chisq_den(x = prop, k = k) / chisq_den(x = out[t-1], k = k)

    U = runif(1)
    if(U <= alpha)  # to decide whether to accept or reject
    {
      out[t] = prop
      acc.prob = acc.prob + 1
    } else {
      out[t] = out[t-1]
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
          tabBox(
            title = "Figure 1", id = "tabset1", width = NULL,
            tabPanel(title = "Animation", plotOutput("mh_dependent"), value = 1),
            tabPanel(title = "Static", plotOutput("mh_dependent_static"), value = 2)
          ),
          tabBox(
            title = "Figure 2", id = "tabset2", width = NULL,
            tabPanel(title = "Animation", plotOutput("mh_independent"), value = 1),
            tabPanel(title = "Static", plotOutput("mh_independent_static"), value = 2)
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
            "Metropolis Hastings algorithm using y = N(x, h) as its proposal distribution where x is the current value of the Markov chain and h is the step size"
          ),
          box(
            title = "Figure 2", width = NULL, status = 'warning',
            "Independent Metropolis Hastings algorithm using y = N(2, h) as its constant proposal distribution where h is the step size"
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
            ),
            numericInput(
              "h_dep", "Step Size for Dependent MH: ", value = 0.5, min = 0.001, max = 10, step = 0.5
            ),
            numericInput(
              "h_indep", "Step Size for Independent MH: ", value = 8^4, min = 1, max = 10^4, step = 100
            )
          ),
          box(
            title = "Control Panel", width = NULL, status = 'primary',
            fluidRow(
              column(6, uiOutput("playButton")),
              column(6, uiOutput("resetButton"))
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
  reps = 1000
  N = 1e2 # number of steps
  k = 10 # df of the target chi-sq distribution
  colors1 = rainbow(n=N/2, start = 1/25, end = 1/8, alpha=0.2)
  colors2 = rainbow(n=N/2, start = 1/1.85, end = 1/1.65, alpha = 0.2)
  colors = c(colors1, colors2)

  chain = reactiveValues()
  chain$values_dep = matrix(0, nrow = reps, ncol = N)
  chain$values_indep = matrix(0, nrow = reps, ncol = N)
  chain$time = 1
  chain$target = rchisq(1e5, df = k)

  control = reactiveValues()
  control$isRunning = 0
  control$isStart = 0

  output$playButton = renderUI({
    if(control$isRunning == 0){
      actionButton("play", "Play", width = '100%')
    } else {
      actionButton("pause", "Pause", width = '100%')
    }
  })
  output$resetButton = renderUI({
    if(control$isStart == 1){
      actionButton("reset", "Reset", width = '100%')
    } else {
      actionButton("start", "Start", width = '100%')
    }
  })

  simulate = function() {
    h_dep = input$h_dep
    h_indep = input$h_indep
    if(input$dist == "stat") {
      random.chi = rchisq(reps, df = k)
      for(r in 1:reps) {
        # Stationary starting value
        chain$values_dep[r, ] = chisq_mh_dep(N = N, k = k, h = h_dep, start = random.chi[r])
        chain$values_indep[r, ] = chisq_mh_indep(N = N, k = k, h = h_indep, start = random.chi[r])
      }
    }
    else if(input$dist == "exp1") {
      random.exp1 <- rexp(reps, 0.05)
      for(r in 1:reps) {
        # Exp(0.05) starting value
        chain$values_dep[r, ] = chisq_mh_dep(N = N, k = k, h = h_dep, start = random.exp1[r])
        chain$values_indep[r, ] = chisq_mh_indep(N = N, k = k, h = h_indep, start = random.exp1[r])
      }
    }
    else if(input$dist == "exp2") {
      random.exp2 = rexp(reps, 0.01)
      for(r in 1:reps) {
        # Exp(0.01) starting value
        chain$values_dep[r, ] = chisq_mh_dep(N = N, k = k, h = h_dep, start = random.exp2[r])
        chain$values_indep[r, ] = chisq_mh_indep(N = N, k = k, h = h_indep, start = random.exp2[r])
      }
    } else {
      for(r in 1:reps) {
        # Fixed at 3 starting value
        chain$values_dep[r, ] = chisq_mh_dep(N = N, k = k, h = h_dep, start = 3)
        chain$values_indep[r, ] = chisq_mh_indep(N = N, k = k, h = h_indep, start = 3)
      }
    }
  }

  forward = function() {
    chain$time = min(chain$time+1, N)
  }

  observeEvent(input$start, {
    control$isRunning = 1
    control$isStart = 1
    simulate()
    observeEvent(if(control$isRunning) {invalidateLater(1000); TRUE}, {
      forward()
    })
  })

  observeEvent(input$play,{
    control$isRunning = 1
    control$isStart = 1
    if(chain$time == 1){
      simulate()
    }
    observeEvent(if(control$isRunning) {invalidateLater(1000); TRUE}, {
      forward()
    })
  })

  observeEvent(input$pause,{
    control$isRunning = 0
  })

  # when reset button is pressed (set everything to original values)
  observeEvent(input$reset,{
    control$isRunning = 0
    control$isStart = 0
    chain$values_dep = matrix(0, nrow = reps, ncol = N)
    chain$values_indep = matrix(0, nrow = reps, ncol = N)
    chain$time = 1
  })
  # main plot
  output$mh_dependent = renderPlot({
    plot(density(chain$target), type = 'l', lwd = 2, main = "Density estimates", ylim = c(0,.15), xlab = paste("Number of draws: ", min(chain$time, N)))
    if(chain$time != 1){
      for(k in 1:chain$time){
        lines(density(chain$values_dep[, k]), col = adjustcolor(colors[N+1-k]))
      }
      lines(density(chain$target), lty=2, lwd=0.8)
      # lines(density(chain$values_dep[, chain$time]), lwd = 2, col = adjustcolor("green"))
    }
    legend("topright", col = c("black", "green", "red"), legend = c("target", "current", "prev"),
           lwd = c(2, 2, 1))
  })
  output$mh_dependent_static = renderPlot({
    plot(density(chain$target), type = 'l', lwd = 2, main = "Density estimates", ylim = c(0, .15))
    if(chain$time != 1){
      for(k in 1:N){
        lines(density(chain$values_dep[, k]), col = adjustcolor(colors[N+1-k]))
      }
      lines(density(chain$target), lty=2, lwd=0.8)
    }
  })
  output$mh_independent = renderPlot({
    plot(density(chain$target), type = 'l', lwd = 2, main = "Density estimates", ylim = c(0,.15), xlab = paste("Number of draws: ", min(chain$time, N)))
    if(chain$time != 1){
      for(k in 1:chain$time){
        lines(density(chain$values_indep[, k]), col = adjustcolor(colors[N+1-k]))
      }
      lines(density(chain$target), lty=2, lwd=0.8)
    }
    legend("topright", col = c("black", "green", "red"), legend = c("target", "current", "prev"),
           lwd = c(2, 2, 1))
  })
  output$mh_independent_static = renderPlot({
    plot(density(chain$target), type = 'l', lwd = 2, main = "Density estimates", ylim = c(0, .15))
    if(chain$time != 1){
      for(k in 1:N){
        lines(density(chain$values_indep[, k]), col = adjustcolor(colors[N+1-k]))
      }
      lines(density(chain$target), lty=2, lwd=0.8)
    }
  })
}

shinyApp(ui, server)
