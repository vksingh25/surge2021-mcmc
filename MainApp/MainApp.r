# Stuff left to do!
"if(FALSE){
  0. TODOs
    0.1 Merge Start and Reset button
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
      "Chi-squared"="chisq",
      "Normal" = "norm",
      "t-Distribution" = "t.dist"
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
  # uiOutput("startButton"),
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
          sliderInput(inputId = "targetAnimation", label = "Animation", min = 1, max = 10, value = 10, animate = animationOptions(interval = 1000)),
          tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
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
          tabPanel(title = "Animation", plotOutput("time_anime"), value = 1),
          tabPanel(title = "Static", plotOutput("time_static"), value = 2)
        ),
      ),
      column(
        width = 3,
        box(
          title = "Slider", width = NULL,
          sliderInput(inputId = "time", label = "Number of Draws", min = 0, max = 100, value = 0, animate = animationOptions(interval = 300)),
          tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
          # tags$div(class="header", checked=NA,
          #      tags$p("Ready to take the Shiny tutorial? If so"),
          #      tags$a(href="shiny.rstudio.com/tutorial", "Click Here!")
          # )
        )
      )
    )
  )
)

ui = dashboardPage(
  dashboardHeader(titleWidth = 250, title = ""),
  sidebar,
  body
)

server = function(input, output) {
  ###########################################
  # variables required throughout the app
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
  # target = rchisq(1e5, df = 10)

# reactive variables
  time = reactive({ input$time })
  h = reactive({ input$h })
  dist = reactive({ input$dist })
  kernel = reactive({ input$kernel })

  control = reactiveValues()
  control$computed = 0

  #######################################
  # variables required for app 1
  #######################################
  density = reactiveValues()
  density$proposal = numeric(length = N)

  #######################################
  # variables required for app 2
  #######################################
  chain = reactiveValues()
  chain$values = matrix(0, nrow = reps, ncol = N)
  chain$target = numeric(1e5)
  # plot variables app2
  plots = reactiveValues()
  plots$mh_anime = list()
  plots$mh_static = list()
  plots$target = ggplot()
  # plots$target = ggplot(data = data.frame(target = target), mapping = aes(x = target)) +
  #   geom_line(stat = 'density', linetype = 'dashed', lwd = 0.75) +
  #   labs(title = "Density estimates from fixed") +
  #   coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.2)) +
  #   theme_classic()


  # returns target densityw
  target_den = function(x, dist){
    rtn = 0
    if(dist == 'chisq') {
      k = 10
      if(x > 0) {
        rtn = x^(k/2 - 1) * exp(- x/2)
      } else {
        rtn = 0
      }
    } else if (dist == 'norm') {
      mean = 0
      sd = 1
      rtn = exp(-((x-mean)/sd)^2/2)
    } else if (dist == 't.dist') {
      k = 10
      rtn = (1 + x^2/k) ^ (-(k+1)/2)
    }
    return (rtn)
  }

  # returns proposal values using selected algorithm
  target_mh = function(N, start = 3, kernel, dist, h){
    out = numeric(length = N)
    acc.prob = 0  # acceptance prob
    out[1] = start
    mean = 2
    for(t in 2:N) {
      if(kernel == 'mh_dep'){
        mean = out[t-1]
      }
      # proposal N(x, h). Use sd = sqrt(variance) in R
      prop = rnorm(1, mean = mean, sd = sqrt(h))

      # the proposal density gets canceled here
      alpha = target_den(x = prop, dist) / target_den(x = out[t-1], dist)

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
    return (out)
  }

  random.dist = function(N, dist){
    if(dist == 'chisq'){
      rtn = rchisq(N, df = 10)
    } else if (dist == 'norm'){
      rtn = rnorm(N, mean = 0, sd = 1)
    } else if (dist == 't.dist'){
      rtn = rt(N, df = 10)
    }
  }

  targetPlot.dist = function(target, dist){
    p = ggplot(data = data.frame(target = target), mapping = aes(x = target)) +
        geom_line(stat = 'density', linetype = 'dashed', lwd = 0.75) +
        labs(title = "Density estimates from fixed") +
        # coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.2)) +
        theme_classic()
    if(dist == 'chisq'){
      p = p + coord_cartesian(xlim = c(0, 50), ylim = c(0, 0.2))
    } else if (dist == 'norm') {
      p = p + coord_cartesian(xlim = c(-8, 8), ylim = c(0, 0.4))
    } else if (dist == 't.dist') {
      p = p + coord_cartesian(xlim = c(-10, 10), ylim = c(0, 0.5))
    }
    return (p)
  }
  density.plots = function(N, start, kernel, dist, h) {
    for(r in 1:reps){
      chain$values[r, ] = target_mh(N = N, start = 3, kernel, dist, h)
      print(r)
    }
    chain$target = random.dist(1e5, dist)
    plots$target = targetPlot.dist(chain$target, dist)
    p = plots$target
    for(i in 1:1e2){
      p = p + geom_line(data = data.frame(output = chain$values[, i]), mapping = aes(x = output), stat = 'density', color = colors_static[N+1-i])
      if(i == N){
        p = p + geom_line(stat = 'density', linetype = 'dashed')
      }
      plots$mh_static[[i]] = p
      plots$mh_anime[[i]] = plots$target + geom_line(data = data.frame(output = chain$values[, i]), mapping = aes(x = output), stat = 'density', color = colors_anime[N+1-i])
    }
  }

  simulate = function() {
    if(!control$computed){
      density$proposal = target_mh(N = 1e4, start = 3, kernel(), dist(), h())
      density.plots(N = N, start = 3, kernel(), dist(), h())
      control$computed = 1
    }

  }

  observeEvent(input$start, {
    if(control$computed == 0){
      simulate()
    }
  })

  observeEvent(input$reset, {
    control$computed = 0
    chain$values = matrix(0, nrow = reps, ncol = N)
    density$proposal = numeric(length = N)
    chain$target = numeric(1e5)
    plots$target = ggplot()
  })

  # output plots of app 1
    output$mh_density = renderPlot({
      if(control$computed){
        ggplot(data = data.frame(output = density$proposal), mapping = aes(x = output, color = 'blue', linetype = 'current')) +
          geom_line(stat = 'density') +
          geom_line(data = data.frame(target = chain$target), mapping = aes(x = target, color = 'black', linetype = 'target'), stat = 'density', lty = 2) +
          scale_color_manual(name = 'Legend', values = c('blue' = 'blue', 'black' = 'black'), labels = c('current', 'target')) +
          scale_linetype_manual(name = 'Legend', values = c('current' = 1, 'target' = 2)) +
          ylab('Density') + xlab("N = 1e4") +
          labs(title = 'Density Plot') +
          theme_classic()
      }
    })
    output$mh_acf = renderPlot({
      if(control$computed){
        acf(density$proposal, main = "ACF Plot")
      }
    })
    output$mh_trace = renderPlot({
      if(control$computed){
        ggplot(data = data.frame(output = density$proposal, time = 1:1e3), mapping = aes(x = time, y = output)) +
          geom_line() +
          xlab("Time") + ylab("Proposal") +
          labs(title = "Trace Plot")
      }
  })

  # output plots for app 2
  output$time_anime = renderPlot({
    if(control$computed){
      if(time() == 0){
        plots$target
      } else {
        plots$mh_anime[[time()]]
      }
    }
  })
  output$time_static = renderPlot({
    if(control$computed){
      if(time() == 0){
        plots$target
      } else {
        plots$mh_static[[time()]]
      }
    }
  })

}

shinyApp(ui, server)
