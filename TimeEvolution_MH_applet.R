# TODO: disable reset button (using shinyjs)

library(shiny)
library(shinydashboard)
library(ggplot2)

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
          width = 8,
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
          width = 4,
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
            sliderInput(
              inputId = 'time',
              label = 'Number of draws',
              min = 0, max = 100, value = 0,
              animate = animationOptions(interval = 750)
            ),
            textOutput("computing"), br(),
            fluidRow(
              column(6, actionButton(inputId = 'start', label = 'Start', width = '100%')),
              column(6, actionButton(inputId = 'reset', label = 'Reset', width = '100%'))
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
  # All the required constants
  reps = 1000
  N = 1e2 # number of steps
  k = 10 # df of the target chi-sq distribution
  colors_red_static = rainbow(n=N/2, start = 1/25, end = 1/8, alpha=0.2)
  colors_blue_static = rainbow(n=N/2, start = 1/1.85, end = 1/1.65, alpha = 0.2)
  colors_static = c(colors_red_static, colors_blue_static)
  colors_red_anime = rainbow(n=N/2, start = 1/25, end = 1/8, alpha=1)
  colors_blue_anime = rainbow(n=N/2, start = 1/1.85, end = 1/1.65, alpha = 1)
  colors_anime = c(colors_red_anime, colors_blue_anime)
  target = rchisq(1e5, df = k)

  chain = reactiveValues()
  chain$values_dep = matrix(0, nrow = reps, ncol = N)
  chain$values_indep = matrix(0, nrow = reps, ncol = N)

  plots = reactiveValues()
  plots$dep_anime = list()
  plots$dep_static = list()
  plots$indep_anime = list()
  plots$indep_static = list()
  plots$target = ggplot(data = data.frame(target = target), mapping = aes(x = target)) +
    geom_line(stat = 'density', linetype = 'dashed', lwd = 0.75) +
    labs(title = "Density estimates from fixed") +
    coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.2)) +
    theme_classic()

  control = reactiveValues()
  control$computed = 0

  simulate = function() {
    dist = input$dist
    h_dep = input$h_dep
    h_indep = input$h_indep

    if(dist == "stat") {
      random.chi = rchisq(reps, df = k)
      for(r in 1:reps) {
        # Stationary starting value
        chain$values_dep[r, ] = chisq_mh_dep(N = N, k = k, h = h_dep, start = random.chi[r])
        chain$values_indep[r, ] = chisq_mh_indep(N = N, k = k, h = h_indep, start = random.chi[r])
      }
    }
    else if(dist == "exp1") {
      random.exp1 <- rexp(reps, 0.05)
      for(r in 1:reps) {
        # Exp(0.05) starting value
        chain$values_dep[r, ] = chisq_mh_dep(N = N, k = k, h = h_dep, start = random.exp1[r])
        chain$values_indep[r, ] = chisq_mh_indep(N = N, k = k, h = h_indep, start = random.exp1[r])
      }
    }
    else if(dist == "exp2") {
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
    p_dep = plots$target
    p_indep = plots$target
    for(i in 1:N){
      p_dep = p_dep + geom_line(data = data.frame(output = chain$values_dep[, i]), mapping = aes(x = output), stat = 'density', color = colors_static[N+1-i])
      p_indep = p_indep + geom_line(data = data.frame(output = chain$values_indep[, i]), mapping = aes(x = output), stat = 'density', color = colors_static[N+1-i])
      if(i == N){
        p_dep = p_dep + geom_line(stat = 'density', linetype = 'dashed')
        p_indep = p_indep + geom_line(stat = 'density', linetype = 'dashed')
      }
      plots$dep_static[[i]] = p_dep
      plots$indep_static[[i]] = p_indep
      plots$dep_anime[[i]] = plots$target + geom_line(data = data.frame(output = chain$values_dep[, i]), mapping = aes(x = output), stat = 'density', color = colors_anime[N+1-i])
      plots$indep_anime[[i]] = plots$target + geom_line(data = data.frame(output = chain$values_indep[, i]), mapping = aes(x = output), stat = 'density', color = colors_anime[N+1-i])
    }
    control$computed = 1
  }

  observeEvent(input$start, {
    simulate()
  })

  # when reset button is pressed (set everything to original values)
  observeEvent(input$reset,{
    chain$values_dep = matrix(0, nrow = reps, ncol = N)
    chain$values_indep = matrix(0, nrow = reps, ncol = N)
    control$computed = 0
  })

  time = reactive({ input$time })

  output$computing = renderText({
    if(control$computed){
      "Values computed, please press Play. To reset values, press Reset"
    } else {
      "To compute values, press Start"
    }
  })
  # main plots
  output$mh_dependent = renderPlot({
    if(time() == 0 || !control$computed){
      plots$target
    } else {
      plots$dep_anime[[time()]]
    }
  })
  output$mh_dependent_static = renderPlot({
    if(time() == 0 || !control$computed){
      plots$target
    } else {
      plots$dep_static[[time()]]
    }
  })
  output$mh_independent = renderPlot({
    if(time() == 0 || !control$computed){
      plots$target
    } else {
      plots$indep_anime[[time()]]
    }
  })
  output$mh_independent_static = renderPlot({
    if(time() == 0 || !control$computed){
      plots$target
    } else {
      plots$indep_static[[time()]]
    }
  })
}

shinyApp(ui, server)
