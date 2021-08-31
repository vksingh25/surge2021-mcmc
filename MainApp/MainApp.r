# Stuff left to do!
# "if(FALSE){
#   0. TODOs
#     0.7 Change target density plots from random draws to using density function
#     0.6 write full app description in about section (starting of the app)
#     0.1 Merge Start and Reset button
#     0.4 Center play button
#     0.5 add progress bar when start is clicked
#     0.2 Return acceptance Probability
#     0.n Use C++ for loops
#   1. Basic MH demo
#     1.5 Description of ACF and TS
#   3. Law of Large Numbers
#   4. CLT
#   5. Credits and stuff
# }"

library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyjs)
library(reshape2)

sidebar = dashboardSidebar(
  width = 250,
  useShinyjs(),
  tags$style(type='text/css', ".selectize-input { font-size: 16px; line-height: 16px;} .selectize-dropdown { font-size: 15px; line-height: 15px; }"),
  tags$head(
      tags$style(type='text/css',
                 ".nav-tabs {font-size: 22px} ")
  ),
  tags$head(
      tags$style(type='text/css',
                 ".box-header h3.box-title {
                    font-size: 22px;
                  } ")
  ),
  selectInput(
    "dist", tags$h4(tags$b("Target Distribution")),
    c(
      "Chi-squared"="chisq",
      "Normal" = "norm",
      "t-Distribution" = "t.dist"
    )
  ),
  numericInput("df_chisq", tags$h4(tags$b("df")), value = 10, min = 1, max = 20, step = 1),
  fluidRow(
    column(6,
      numericInput("mean_norm", tags$h4(tags$b("Mean")), value = 0, min = -10, max = 10),
    ),
    column(6,
      numericInput("sd_norm", tags$h4(tags$b("SD")), value = 1, min = 0.01, max = 10),
    )
  ),
  numericInput("df_t", tags$h4(tags$b("df")), value = 10, min = 0.01, max = 10),
  selectInput(
    "kernel", tags$h4(tags$b("Update Mechanism")),
    c(
      "Gaussian MH" = "mh_dep",
      "Independent MH" = "mh_indep"
    )
  ),
  numericInput("h", tags$h4(tags$b("Step Size")), value = 100, min = 0.05, max = 1000),
  selectInput(
    "starting_dist", tags$h4(tags$b("Starting Distribution")),
    c(
      "Fixed at 3" = "fixed",
      "Exp(0.05)" = "exp1",
      "Exp(0.01)" = "exp2"
    )
  ),
  # uiOutput("startButton"),
  actionButton(inputId = 'start', label = 'Start', width = 220),
  actionButton(inputId = 'reset', label = 'Reset', width = 220)
)

body = dashboardBody(
  fluidPage(
    box(
      title = 'About the app', width = NULL, status = 'primary',
      # Everything about the app, how to use it, etc
      textOutput("about_app"),
      tags$head(tags$style("#about_app{
                             font-size: 16px;
                            }"
        )
      )
    ),
    box(
      title = 'About Metropolis Hastings algorithm', width = NULL, status = 'primary',
      # Everything about the app, how to use it, etc
      textOutput("algo_desc"),
      tags$head(tags$style("#algo_desc{
                             font-size: 16px;
                            }"
        )
      )
    ),
    fluidRow(
      column(
        width = 9,
        tabBox(
          title = "MH Plot", id = "tabset_app1", width = NULL,
          tabPanel(title = "Static", plotOutput("mh_density"), value = 1),
          tabPanel(title = "Animation", plotOutput("mh_density_anime"), value = 2)
        )
      ),
      column(
        width = 3,
        box(
          title = "About Target", width = NULL,
          textOutput("aboutTarget"),
          tags$head(tags$style("#aboutTarget{
                             font-size: 16px;
                            }"
            )
          )
        ),
        box(
          title = "Slider", width = NULL,
          sliderInput(inputId = "targetAnimation", label = "Animation", min = 1, max = 19, value = 1, animate = animationOptions(interval = 1500)),
          tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "ACF Plot", width = NULL,
          textOutput("aboutACF"),
          tags$head(tags$style("#aboutTarget{
                             font-size: 16px;
                            }"
            )
          ),
          plotOutput("mh_acf")
        )
      ),
      column(
        width = 6,
        box(
          title = "Trace Plot", width = NULL,
          textOutput("aboutTrace"),
          tags$head(tags$style("#aboutTarget{
                             font-size: 16px;
                            }"
            )
          ),
          plotOutput("mh_trace")
        )
      )
    ),
    box(
      title = 'About Time Evolution', width = NULL, status = 'primary',
      # Everything about selected algorithm
      textOutput("timeDesc"),
      tags$head(tags$style("#timeDesc{
                             font-size: 16px;
                            }"
        )
      )
    ),

    box(
      title = "About Stationarity", width = NULL, status = 'primary',
      textOutput("aboutStationarity"),
      tags$head(tags$style("#aboutStationarity{
                          font-size: 16px;
                        }"
        )
      )
    ),
    fluidRow(
      column(
        width = 9,
        tabBox(
          title = "Stationary", id = "tabset_app2.1", width = NULL,
          tabPanel(title = "Animation", plotOutput("time_anime_stat"), value = 1),
          tabPanel(title = "Static", plotOutput("time_static_stat"), value = 2)
        )
      ),
      column(
        width = 3,
        box(
          title = "Slider", width = NULL,
          sliderInput(inputId = "time_stat", label = "Number of Draws", min = 0, max = 100, value = 0, animate = animationOptions(interval = 750)),
          tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }"))
        )
      )
    ),
    box(
      title = "About Ergodicity", width = NULL, status = 'primary',
      textOutput("aboutErgodicity"),
      tags$head(tags$style("#aboutErgodicity{
                          font-size: 16px;
                        }"
        )
      )
    ),
    fluidRow(
      column(
        width = 9,
        tabBox(
          title = "Ergodic", id = "tabset_app2.2", width = NULL,
          tabPanel(title = "Animation", plotOutput("time_anime"), value = 1),
          tabPanel(title = "Static", plotOutput("time_static"), value = 2)
        )
      ),
      column(
        width = 3,
        box(
          title = "Slider", width = NULL,
          sliderInput(inputId = "time_erg", label = "Number of Draws", min = 0, max = 100, value = 0, animate = animationOptions(interval = 750)),
          tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }"))
        )
      )
    ),
    box(
      title = 'About Strong Law of Large Numbers', width = NULL, status = 'primary',
      textOutput("aboutSLLN"),
      tags$head(tags$style("#aboutSLLN{
                          font-size: 16px;
                        }"
        )
      )
    ),
    box(
      title = 'Strong Law of Large Numbers', id = 'slln', width = NULL,
      plotOutput("slln")
    ),
    box(
      title = 'Central Limit Theorem in Markov chains', width = NULL, status = 'primary',
      textOutput("aboutCLT"),
      tags$head(tags$style("#aboutCLT{
                          font-size: 16px;
                        }"
        )
      )
    ),
    box(
      title = 'Central Limit Theorem', id = 'clt', width = NULL,
      plotOutput('clt')
    )
  )
)

ui = dashboardPage(
  dashboardHeader(titleWidth = 250, title = ""),
  sidebar,
  body
)

server = function(input, output) {

  observeEvent(input$dist, {
    switch(input$dist,
      'chisq' = {
          shinyjs::hide("mean_norm")
          shinyjs::hide("sd_norm")
          shinyjs::show("df_chisq")
          shinyjs::hide("df_t")
      },
      'norm' = {
        shinyjs::show("mean_norm")
        shinyjs::show("sd_norm")
        shinyjs::hide("df_t")
        shinyjs::hide("df_chisq")
      },
      't.dist' = {
        shinyjs::hide("mean_norm")
        shinyjs::hide("sd_norm")
        shinyjs::hide("df_chisq")
        shinyjs::show("df_t")
      })
  })
  # variables required throughout the app
  reps_chain = 1e3
  N_chain = 1e2
  N_anime = 20
  colors_red_static = rainbow(n=N_chain/2, start = 1/25, end = 1/8, alpha=0.2)
  colors_blue_static = rainbow(n=N_chain/2, start = 1/1.85, end = 1/1.65, alpha = 0.2)
  colors_static = c(colors_red_static, colors_blue_static)
  colors_red_anime = rainbow(n=N_chain/2, start = 1/25, end = 1/8, alpha=1)
  colors_blue_anime = rainbow(n=N_chain/2, start = 1/1.85, end = 1/1.65, alpha = 1)
  colors_anime = c(colors_red_anime, colors_blue_anime)
  reps_lln = 50
  reps_clt = 1000
  size_lln.clt = 1000

  # reactive variables
  time_stat = reactive({ input$time_stat })
  time_erg = reactive({ input$time_erg })
  targetAnimation = reactive({ input$targetAnimation })
  h = reactive({ input$h })
  dist = reactive({ input$dist })
  kernel = reactive({ input$kernel })
  parameters = reactive({
    x = list(
      df_chisq = input$df_chisq,
      df_t = input$df_t,
      mean_norm = input$mean_norm,
      sd_norm = input$sd_norm
    )
  })
  starting_dist = reactive({ input$starting_dist })

  control = reactiveValues()
  control$computed = 0

  # variables required for app 1
  density = reactiveValues()
  density$proposal = numeric(length = N_chain)
  density$acc.prob = 1
  density$acc = numeric(length = N_anime)
  density$prop = numeric(length = N_anime)
  density$samp = numeric(length = N_anime)
  density$plots = list()

  # variables required for app 2
  chain = reactiveValues()
  chain$values = matrix(0, nrow = reps_chain, ncol = N_chain)
  chain$values_stat = matrix(0, nrow = reps_chain, ncol = N_chain)
  chain$target = numeric(1e5)

  lln.clt = reactiveValues()
  lln.clt$values = matrix(0, nrow = size_lln.clt, ncol = reps_clt)
  lln.clt$runningMean = matrix(0, nrow = 1e3*5, ncol = reps_lln)
  lln.clt$mean = 10

  # plot variables for app 2
  plots = reactiveValues()
  plots$mh_anime_stat = list()
  plots$mh_static_stat = ggplot()
  plots$mh_anime = list()
  plots$mh_static = ggplot()
  plots$target = ggplot()

  # returns target density
  target_den = function(x, dist, parameters){
    rtn = 0
    if(dist == 'chisq') {
      k = parameters$df_chisq
      if(x > 0) {
        rtn = x^(k/2 - 1) * exp(- x/2)
      } else {
        rtn = 0
      }
    } else if (dist == 'norm') {
      mean = parameters$mean_norm
      sd = parameters$sd_norm
      rtn = exp(-((x-mean)/sd)^2/2)
    } else if (dist == 't.dist') {
      k = parameters$df_t
      rtn = (1 + x^2/k) ^ (-(k+1)/2)
    }
    return (rtn)
  }

  # returns proposal values using selected algorithm
  # TODO: correct the independent MH function
  target_mh = function(N, start = 3, kernel, dist, h, parameters, acc_prob = FALSE){
    if(acc_prob){
      out = numeric(length = N+1)
    } else {
      out = numeric(length = N)
    }
    acc.prob = 1  # acceptance prob
    out[1] = start
    mean = 2
    for(t in 2:N) {
      if(kernel == 'mh_dep'){
        mean = out[t-1]
      }
      # proposal N(x, h). Use sd = sqrt(variance) in R
      prop = rnorm(1, mean = mean, sd = sqrt(h))

      # the proposal density gets canceled here
      alpha = target_den(x = prop, dist, parameters) / target_den(x = out[t-1], dist, parameters)

      U = runif(1)
      if(U <= alpha)  # to decide whether to accept or reject
      {
        out[t] = prop
        acc.prob = acc.prob + 1
      } else {
        out[t] = out[t-1]
      }
    }
    if(acc_prob){
      out[N+1] = acc.prob / N
      print(acc.prob/N)
    }
    return (out)
  }

  # returns accept reject animation plots
  acceptReject_mh = function(N_anime, start = 3, kernel, dist, h, parameters){
    print("accept reject mh animation")
    samp = numeric(length = N_anime)
    prop = numeric(length = N_anime)
    acc = numeric(length = N_anime)

    acc[1] = 1
    samp[1] = start
    prop[1] = start
    mean = 2
    for(t in 2:N){
      if(kernel == 'mh_dep'){
        mean = samp[t-1]
      }
      prop[t] = rnorm(1, mean = mean, sd = sqrt(h))
      alpha = target_den(x = prop[t], dist, parameters) / target_den(x = samp[t-1], dist, parameters)
      U = runif(1)
      if(U <= alpha){
        samp[t] = prop[t]
        acc[t] = 1
      } else {
        samp[t] = samp[t-1]
        acc[t] = 0
      }
    }
    colors = ifelse(acc, "blue", "red")
    target_curve = random.dist(1e4, dist, parameters)
    plots = list()
    plots[[1]] = ggplot(data = data.frame(target = target_curve), mapping = aes(x = target)) +
      geom_line(stat = 'density', lty = 2) +
      geom_point(data = data.frame(x = prop[1], y = numeric(length = 1)), mapping = aes(x = x, y = y), colour = 'green', size = 3) +
      scale_color_manual(name = "Legend", values = c('blue' = 'blue', 'red' = 'red'), labels = c('red' = 'reject', 'blue' = 'accept')) +
      coord_cartesian(xlim = c(-20, 50), ylim = c(0, 0.2)) +
      theme_classic()
    counter = 2
    for(i in 2:N_anime){
      mean = 2
      if(kernel == 'mh_dep'){
        mean = samp[i-1]
      }
      prop_curve = rnorm(1e5, mean = mean, sd = sqrt(h))
      plots[[counter]] = local({
        t = i
        p_inter = ggplot(data = data.frame(target = target_curve), mapping = aes(x = target)) +
          geom_line(stat = 'density', lty = 2) +
          geom_point(data = data.frame(x = prop[1:t-1], y = numeric(length = t-1)), mapping = aes(x = x, y = y), colour = colors[1:t-1]) +
          scale_color_manual(name = "Legend", values = c('blue' = 'blue', 'red' = 'red'), labels = c('red' = 'reject', 'blue' = 'accept')) +
          coord_cartesian(xlim = c(-20, 50), ylim = c(0, 0.2)) +
          theme_classic() +
          geom_line(data = data.frame(target = prop_curve), mapping = aes(x = target), stat = 'density', color = 'grey') +
          geom_point(mapping = aes(x = prop[t], y = 0), colour = 'grey', size = 3)
      })
      color_curr = ifelse(acc[i], 'green', 'red')
      plots[[counter + 1]] = local({
        t = i
        p_ar = ggplot(data = data.frame(target = target_curve), mapping = aes(x = target)) +
          geom_line(stat = 'density', lty = 2) +
          geom_point(data = data.frame(x = prop[1:t-1], y = numeric(length = i-1)), mapping = aes(x = x, y = y), colour = colors[1:t-1]) +
          scale_color_manual(name = "Legend", values = c('blue' = 'blue', 'red' = 'red'), labels = c('red' = 'reject', 'blue' = 'accept')) +
          coord_cartesian(xlim = c(-20, 50), ylim = c(0, 0.2)) +
          theme_classic() +
          geom_line(data = data.frame(target = prop_curve), mapping = aes(x = target), stat = 'density', color = color_curr) +
          geom_point(mapping = aes(x = prop[t], y = 0), colour = color_curr, size = 3)
      })
      counter = counter + 2
    }
    return (plots)
  }

  random.dist = function(N, dist, parameters){
    if(dist == 'chisq'){
      rtn = rchisq(N, df = parameters$df_chisq)
    } else if (dist == 'norm'){
      rtn = rnorm(N, mean = parameters$mean_norm, sd = parameters$sd_norm)
    } else if (dist == 't.dist'){
      rtn = rt(N, df = parameters$df_t)
    }
    return (rtn)
  }

  starting.draw = function(starting_dist){
    if(starting_dist == 'fixed'){
      rtn = 3
    } else if (starting_dist == 'exp1'){
      rtn = rexp(1, rate = 0.05)
    } else if (starting_dist == 'exp2'){
      rtn = rexp(1, rate = 0.01)
    }
    return (rtn)
  }

  targetPlot.dist = function(target, dist){
    p = ggplot(data = data.frame(target = target), mapping = aes(x = target)) +
        geom_line(stat = 'density', linetype = 'dashed', lwd = 0.75) +
        labs(title = "Density estimates from fixed") +
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

  density.plots = function(N, start, kernel, dist, h, parameters) {
    print("time evolution density plots")
    for(r in 1:reps_chain){
      chain$values[r, ] = target_mh(N = N, start = start, kernel, dist, h, parameters, acc_prob = FALSE)
      chain$values_stat[r, ] = target_mh(N = N, start = random.dist(1, dist, parameters), kernel, dist, h, parameters, acc_prob = FALSE)
    }
    chain$target = random.dist(1e5, dist, parameters)
    plots$target = targetPlot.dist(chain$target, dist)
    p = plots$target
    p_stat = plots$target
    for(i in 1:1e2){
      p = p + geom_line(data = data.frame(output = chain$values[, i]), mapping = aes(x = output), stat = 'density', color = colors_static[N+1-i])
      p_stat = p_stat + geom_line(data = data.frame(output = chain$values_stat[, i]), mapping = aes(x = output), stat = 'density', color = colors_static[N+1-i])
      if(i == N){
        p = p + geom_line(stat = 'density', linetype = 'dashed')
        p_stat = p_stat + geom_line(stat = 'density', linetype = 'dashed')
      }
      plots$mh_anime[[i]] = plots$target + geom_line(data = data.frame(output = chain$values[, i]), mapping = aes(x = output), stat = 'density', color = colors_anime[N+1-i])
      plots$mh_anime_stat[[i]] = plots$target + geom_line(data = data.frame(output = chain$values_stat[, i]), mapping = aes(x = output), stat = 'density', color = colors_anime[N+1-i])
    }
    plots$mh_static_stat = p_stat
    plots$mh_static = p
  }

  calculateMeanDistribution = function(dist, parameters){
    if(dist == 'chisq'){
      rtn = parameters$df_chisq
    } else if (dist == 'norm'){
      rtn = parameters$mean_norm
    } else if (dist == 't.dist'){
      rtn = 0
    }
    return (rtn)
  }

  drawIndependentChains = function(N, reps, start, kernel, dist, h, parameters){
    print("draw Inpependent chains")
    chains = matrix(0, nrow = N, ncol = reps)
    for(r in 1:reps){
      chains[, r] = target_mh(N, start, kernel, dist, h, parameters, acc_prob = FALSE)
    }
    return (chains)
  }

  calculateRunningMean = function(N, reps, chains){
    print("running mean calculation")
    running.mean = matrix(0, nrow = N, ncol = reps)
    running.mean[1, ] = chains[1, ]
    for(i in 2:N){
      running.mean[i, ] = colMeans(x = chains[1:i, ])
    }
    return(running.mean)
  }

  simulate = function() {
    if(!control$computed){
      print("Simulation Started!!")
      samp_size = 1e4
      proposed_and_acc = target_mh(N = 1e4, start = starting.draw(starting_dist()), kernel(), dist(), h(), parameters(), acc_prob = TRUE)
      density$proposal = proposed_and_acc[-samp_size+1]
      density$acc.prob = proposed_and_acc[samp_size+1]
      density.plots(N = N_chain, start = starting.draw(starting_dist()), kernel(), dist(), h(), parameters())
      control$computed = 1
      density$plots = acceptReject_mh(N_anime = 20, start = starting.draw(starting_dist()), kernel(), dist(), h(), parameters())
      lln.clt$values = drawIndependentChains(N = size_lln.clt, reps = reps_clt, start = starting.draw(starting_dist()), kernel(), dist(), h(), parameters())
      lln.clt$runningMean = calculateRunningMean(N = size_lln.clt, reps = reps_lln, chains = lln.clt$values[, 1:reps_lln])
      print("Simulation Complete!!")
    }
  }

  observeEvent(input$start, {
    if(control$computed == 0){
      simulate()
    }
  })

  observeEvent(input$reset, {
    control$computed = 0
    chain$values = matrix(0, nrow = reps_chain, ncol = N_chain)
    chain$values_stat = matrix(0, nrow = reps_chain, ncol = N_chain)
    density$proposal = numeric(length = N_chain)
    density$acc.prob = 1
    chain$target = numeric(1e5)
    plots$target = ggplot()
    lln.clt$values = matrix(0, nrow = size_lln.clt, ncol = reps_clt)
    lln.clt$runningMean = matrix(0, nrow = size_lln.clt, ncol = reps_lln)
  })

  # output plots of app 1
    output$about_app = renderText({
      # Learn how to write HTML
      # Need to learn how to include LaTeX
      paste("Markov chain Monte Carlo is a technique that is used to draw samples from complicated probability distributions which can be of very high dimensions. Often we only have access to the unnormalised probability density function of our target distribution.
        Our target is to draw samples from these distributions that are close to being independent and identically distributed.
        Our applet tries to motivate the idea behind MCMC by applying algoritms on well known distributions.
        We have a series of plots in this applet starting from demonstrating the convergence of Metropolis-Hastings algorithm and an animation that shows how it actually works. The next two plots, Autocorrelation function and Trace plot, which shows how \"good\" our draws actually are. The next plot aims to introduce the concept of stationarity and ergodicity of Markov chains. After that we have two more plots which demonstrates two very important theorems, the Strong Law of Large Numbers and the Central Limit Theorem.")
    })

    output$algo_desc = renderText({
      if(kernel() == "mh_dep"){
        paste("Our aim is to produce samples from our selected target distribution. We use the Metropolis-Hastings algorithm to accomplish this task.
          This algorithm simulates an ergodic Markov chain, i.e., the steady state of the Markov chain doesn't depend on the initial state. This simulated chain eventually gives samples similar to draws from our target distribution.
          In the Static tab, you can see how the density of our Markov chain looks similar to the target distribution.
          In the Animation tab, we have tried to demontrate the working of the MH-algorithm. Every draw is a two step process. First we propose a value from a transition kernel (in this case, N(x,", h(), "), where x is the current value of the Markov chain), which is shown in grey color. Then our algorithm either accepts that value, coloring that point green, or it rejects that value, coloring it red. The accept-reject step is based on the MH-ratio of the proposal."

        )
      } else if (kernel() == "mh_indep") {
        paste("Our aim is to produce samples from our selected target distribution. We use the Metropolis-Hastings algorithm to accomplish this task.
          This algorithm simulates an ergodic Markov chain, i.e., the steady state of the Markov chain doesn't depend on the initial state. This simulated chain eventually gives samples similar to draws from our target distribution.
          In the Static tab, you can see how the density of our Markov chain looks similar to the target distribution.
          In the Animation tab, we have tried to demontrate the working of the MH-algorithm. Every draw is a two step process. First we propose a value from a transition kernel (in this case, N(2,", h(), ")), which is shown in grey color. Then our algorithm either accepts that value, coloring that point green, or it rejects that value, coloring it red. The accept-reject step is based on the MH-ratio of the proposal."

        )
      }
    })

    output$mh_density = renderPlot({
      if(control$computed){
        ggplot(data = data.frame(output = density$proposal), mapping = aes(x = output, color = 'blue', linetype = 'current')) +
          geom_line(stat = 'density') +
          geom_line(data = data.frame(target = chain$target), mapping = aes(x = target, color = 'black', linetype = 'target'), stat = 'density', lty = 2) +
          scale_color_manual(name = 'Legend', values = c('blue' = 'blue', 'black' = 'black'), labels = c('current', 'target')) +
          scale_linetype_manual(name = 'Legend', values = c('current' = 1, 'target' = 2)) +
          ylab('Density') + xlab(paste("N =", N_chain)) +
          labs(title = 'Density Plot') +
          theme_classic()
      }
    })

    output$mh_density_anime = renderPlot({
      if(control$computed){
        density$plots[[targetAnimation()]]
      }
    })

    output$aboutTarget = renderText({
      if(dist() == 'chisq') {
        paste("Chi-squared distribution with ", parameters()$df_chisq, " degrees of freedom")
      } else if (dist() == 'norm') {
        paste("Normal distribution with mean ", parameters()$mean_norm, " and standard deviation ", parameters()$sd_norm)
      } else if (dist() == 't.dist') {
        paste("t-distribution with ", parameters()$df_chisq, " degrees of freedom")
      }
    })

    output$aboutACF = renderText({
      paste("")
    })

    output$mh_acf = renderPlot({
      if(control$computed){
        acf(density$proposal, main = "ACF Plot")
      }
    })

    output$aboutTrace = renderText({
      paste("")
    })

    output$mh_trace = renderPlot({
      if(control$computed){
        ggplot(data = data.frame(output = density$proposal, time = 1:reps_chain/2), mapping = aes(x = time, y = output)) +
          geom_line() +
          xlab("Time") + ylab("Proposal") +
          labs(title = "Trace Plot") +
          theme_classic()
      }
  })

  # output plots for app 2
  output$timeDesc = renderText({
    paste("This plot aims to demonstrate the convergence of proposal distribution to target distribution with time.
      We sample 1000 different and independent Markov chains from the selected starting distribution and run each of them for 100 iterations.
      Then we plot the marginal density plot of t-th point of the chain using the 1000 different replications.
      The first tab shows the animation of how the marginal density evolves with time.
      The second tab shows all the densities from the start till time t for better visualization of the convergence."
    )
  })

  output$time_anime_stat = renderPlot({
    if(control$computed){
      if(time_stat() == 0){
        plots$target
      } else {
        plots$mh_anime_stat[[time_stat()]]
      }
    }
  })

  output$time_static_stat = renderPlot({
    if(control$computed){
      plots$mh_static_stat
    }
  })

  output$aboutStationarity = renderText({
    paste("A Markov chain is said to be stationary if the marginal distribution of X_n doesn't depend on n, i.e. every value of the Markov chain has the same distribution as the target distribution.
      It is possible if and only if the intial draw is from the target distribution itself. Using the MH-algorithm, it is guarunteed that if the initial distribution is same as the target distribution, the Markov chain will be stationary.
      All the values of the chain will be identically distributed but won't be independent. There will be some correlation between them depending on the parameters of your aalgorithm.
      Stationarity can be seen in the plot below, density of the independent Markov chains look similar to the target distribution at every time step. They are overlapping exactly as we have taken only a finite number of chains.")
  })

  output$time_anime = renderPlot({
    if(control$computed){
      if(time_erg() == 0){
        plots$target
      } else {
        plots$mh_anime[[time_erg()]]
      }
    }
  })

  output$time_static = renderPlot({
    if(control$computed){
      plots$mh_static
    }
  })

  output$aboutErgodicity = renderText({
    paste("A Markov chain is said to be ergodic if the marginal distribution of X_n converges to the target distribution as n goes to infinity.
      You can observe ergodicity in the plot below, where the density of the independent chains keep getting closer and closer to the target distribution.
      A stationary Markov chain is always ergodic, as it has already converged to the target, but the reverse is not true. If we start from a different initial distribution (which usually is the case), we can never exactly draw from the target.
      We can only draw from something very similar to our target, where similarity to the target depends on the algorithm and number of time steps.")
  })

  output$aboutSLLN = renderText({
    paste("")
  })

  output$slln = renderPlot({
    if(control$computed){
      N = size_lln.clt
      reps = reps_lln
      df = data.frame(data = lln.clt$runningMean, draws = 1:N)
      for(i in 1:reps){
        colnames(df)[i] = paste("Chain", i)
      }
      df1 = melt(df, id.vars = 'draws', variable.name = 'Chains')
      ggplot(df1, mapping = aes(draws, value)) +
        geom_line(aes(color = Chains)) +
        geom_line(mapping = aes(y = lln.clt$mean), lty = 2, size = 1) +
        theme_classic() +
        theme(legend.position = 'none')
    }
  })

  output$clt = renderPlot({
    if(control$computed){
      N = size_lln.clt
      reps = reps_clt
      means = colMeans(lln.clt$values)
      cltError = data.frame(mean = sqrt(reps)*(means - lln.clt$mean))
      ggplot(data = cltError, aes(x = mean)) +
        geom_histogram(aes(y = ..density..), binwidth = 1.25, alpha = .9, color = "#63BCC9", fill = "#B5EAD7", size = 0.4) +
        geom_density(alpha = 1, size = 1) +
        geom_vline(aes(xintercept = 0), lty = 2, size = 1) +
        theme_classic()
    }
  })
}

shinyApp(ui, server)
