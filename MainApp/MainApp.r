# Stuff left to do!
"if(FALSE){
  0. TODOs
    0.7 Change target density plots from random draws to using density function
    0.6 write full app description in about section (starting of the app)
    0.1 Merge Start and Reset button
    0.4 Center play button
    0.5 add progress bar when start is clicked
    0.2 Return acceptance Probability
    0.n Use C++ for loops
  1. Basic MH demo
    1.5 Description of ACF and TS
  3. Law of Large Numbers
  4. CLT
  5. Credits and stuff
}"

library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyjs)
library(reshape2)

sidebar = dashboardSidebar(
  width = 250,
  useShinyjs(),
  selectInput(
    "dist", "Target Distribution",
    c(
      "Chi-squared"="chisq",
      "Normal" = "norm",
      "t-Distribution" = "t.dist"
    )
  ),
  numericInput("df_chisq", "df", value = 10, min = 1, max = 20, step = 1),
  fluidRow(
    column(6,
      numericInput("mean_norm", "Mean", value = 0, min = -10, max = 10),
    ),
    column(6,
      numericInput("sd_norm", "SD", value = 1, min = 0.01, max = 10),
    )
  ),
  numericInput("df_t", "df", value = 10, min = 0.01, max = 10),
  selectInput(
    "kernel", "Update Mechanism",
    c(
      "Gaussian Metropolis Hastings" = "mh_dep",
      "Independent MH" = "mh_indep"
    )
  ),
  numericInput("h", "Step Size", value = 100, min = 0.05, max = 1000),
  selectInput(
    "starting_dist", "Starting Distribution",
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
      textOutput("about_app")
    ),
    box(
      title = 'About Markov chain Monte Carlo Algorithm', width = NULL, status = 'primary',
      # Everything about the app, how to use it, etc
      textOutput("algo_desc")
    ),
    fluidRow(
      column(
        width = 9,
        tabBox(
          title = "MH_plot", id = "tabset_app1", width = NULL,
          tabPanel(title = "Static", plotOutput("mh_density"), value = 1),
          tabPanel(title = "Animation", plotOutput("mh_density_anime"), value = 2)
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
          sliderInput(inputId = "targetAnimation", label = "Animation", min = 1, max = 20, value = 1, animate = animationOptions(interval = 1000)),
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
          title = "Stationary", id = "tabset_app2.1", width = NULL,
          tabPanel(title = "Animation", plotOutput("time_anime_stat"), value = 1),
          tabPanel(title = "Static", plotOutput("time_static_stat"), value = 2)
        )
      ),
      column(
        width = 3,
        box(
          title = "About Stationarity", width = NULL,
          "All about stationarity",
        ),
        box(
          title = "Slider", width = NULL,
          sliderInput(inputId = "time_stat", label = "Number of Draws", min = 0, max = 100, value = 0, animate = animationOptions(interval = 350)),
          tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
        )
      ),
      column(
        width = 9,
        tabBox(
          title = "Ergodic", id = "tabset_app2.2", width = NULL,
          tabPanel(title = "Animation", plotOutput("time_anime"), value = 1),
          tabPanel(title = "Static", plotOutput("time_static"), value = 2)
        ),
      ),
      column(
        width = 3,
        box(
          title = "About Ergodicity", width = NULL,
          "All about ergodicity",
        ),
        box(
          title = "Slider", width = NULL,
          sliderInput(inputId = "time", label = "Number of Draws", min = 0, max = 100, value = 0, animate = animationOptions(interval = 350)),
          tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
        )
      )
    ),
    box(
      title = 'About Strong Law of Large Numbers', width = NULL, status = 'primary',
      "Strong law of large numbers"
    ),
    box(
      title = 'Strong Law of Large Numbers', id = 'slln', width = NULL,
      "How to read slln curves",
      plotOutput("slln")
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
  reps = 1e3
  N = 1e2
  N_anime = 20
  colors_red_static = rainbow(n=N/2, start = 1/25, end = 1/8, alpha=0.2)
  colors_blue_static = rainbow(n=N/2, start = 1/1.85, end = 1/1.65, alpha = 0.2)
  colors_static = c(colors_red_static, colors_blue_static)
  colors_red_anime = rainbow(n=N/2, start = 1/25, end = 1/8, alpha=1)
  colors_blue_anime = rainbow(n=N/2, start = 1/1.85, end = 1/1.65, alpha = 1)
  colors_anime = c(colors_red_anime, colors_blue_anime)

  # reactive variables
  time = reactive({ input$time })
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
  density$proposal = numeric(length = N)
  density$acc.prob = 1
  density$acc = numeric(length = N_anime)
  density$prop = numeric(length = N_anime)
  density$samp = numeric(length = N_anime)
  density$plots = list()

  # variables required for app 2
  chain = reactiveValues()
  chain$values = matrix(0, nrow = reps, ncol = N)
  chain$values_stat = matrix(0, nrow = reps, ncol = N)
  chain$target = numeric(1e5)

  lln = reactiveValues()
  lln$values = matrix(0, nrow = 1e3*5, ncol = 10)
  lln$runningMean = matrix(0, nrow = 1e3*5, ncol = 10)
  lln$mean = 10

  # plot variables for app 2
  plots = reactiveValues()
  plots$mh_anime_stat = list()
  plots$mh_static_stat = list()
  plots$mh_anime = list()
  plots$mh_static = list()
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
      geom_point(data = data.frame(x = prop[1], y = numeric(length = 1)), mapping = aes(x = x, y = y), colour = 'green') +
      scale_color_manual(name = "Legend", values = c('blue' = 'blue', 'red' = 'red'), labels = c('red' = 'reject', 'blue' = 'accept')) +
      coord_cartesian(xlim = c(-20, 50), ylim = c(0, 0.2)) +
      theme_classic()
    counter = 2
    for(i in 2:N_anime){
      p_temp = ggplot(data = data.frame(target = target_curve), mapping = aes(x = target)) +
        geom_line(stat = 'density', lty = 2) +
        geom_point(data = data.frame(x = prop[1:i-1], y = numeric(length = i-1)), mapping = aes(x = x, y = y), colour = colors[1:i-1]) +
        scale_color_manual(name = "Legend", values = c('blue' = 'blue', 'red' = 'red'), labels = c('red' = 'reject', 'blue' = 'accept')) +
        coord_cartesian(xlim = c(-20, 50), ylim = c(0, 0.2)) +
        theme_classic()
      mean = 2
      if(kernel == 'mh_dep'){
        mean = samp[i-1]
      }
      prop_curve = rnorm(1e5, mean = mean, sd = sqrt(h))
      plots[[counter]] = p_temp +
        geom_line(data = data.frame(target = prop_curve), mapping = aes(x = target), stat = 'density', color = 'grey') +
        geom_point(mapping = aes(x = prop[i], y = 0), colour = 'grey')
      counter = counter + 1
      color_curr = ifelse(acc[i], 'green', 'red')
      plots[[counter]] = p_temp +
        geom_line(data = data.frame(target = prop_curve), mapping = aes(x = target), stat = 'density', color = color_curr) +
        geom_point(mapping = aes(x = prop[i], y = 0), colour = color_curr)
      counter = counter + 1
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
    for(r in 1:reps){
      chain$values[r, ] = target_mh(N = N, start = start, kernel, dist, h, parameters, acc_prob = FALSE)
      chain$values_stat[r, ] = target_mh(N = N, start = random.dist(1, dist, parameters), kernel, dist, h, parameters, acc_prob = FALSE)
      print(r)
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
      plots$mh_static[[i]] = p
      plots$mh_anime[[i]] = plots$target + geom_line(data = data.frame(output = chain$values[, i]), mapping = aes(x = output), stat = 'density', color = colors_anime[N+1-i])
      plots$mh_static_stat[[i]] = p_stat
      plots$mh_anime_stat[[i]] = plots$target + geom_line(data = data.frame(output = chain$values_stat[, i]), mapping = aes(x = output), stat = 'density', color = colors_anime[N+1-i])
    }
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
    chains = matrix(0, nrow = N, ncol = reps)
    for(r in 1:reps){
      chains[, r] = target_mh(N, start, kernel, dist, h, parameters, acc_prob = FALSE)
    }
    return (chains)
  }

  calculateRunningMean = function(N, reps, chains){
    running.mean = matrix(0, nrow = N, ncol = reps)
    running.mean[1, ] = chains[1, ]
    for(i in 2:N){
      running.mean[i, ] = colMeans(x = chains[1:i, ])
    }
    return(running.mean)
  }

  simulate = function() {
    if(!control$computed){
      samp_size = 1e4
      proposed_and_acc = target_mh(N = 1e4, start = starting.draw(starting_dist()), kernel(), dist(), h(), parameters(), acc_prob = TRUE)
      density$proposal = proposed_and_acc[-samp_size+1]
      density$acc.prob = proposed_and_acc[samp_size+1]
      density.plots(N = N, start = starting.draw(starting_dist()), kernel(), dist(), h(), parameters())
      control$computed = 1
      density$plots = acceptReject_mh(N_anime = 20, start = starting.draw(starting_dist()), kernel(), dist(), h(), parameters())
      lln$values = drawIndependentChains(N = 1e3*5, reps = 10, start = starting.draw(starting_dist()), kernel(), dist(), h(), parameters())
      lln$runningMean = calculateRunningMean(N = 1e3*5, reps = 10, chains = lln$values)
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
    chain$values_stat = matrix(0, nrow = reps, ncol = N)
    density$proposal = numeric(length = N)
    density$acc.prob = 1
    chain$target = numeric(1e5)
    plots$target = ggplot()
    lln$values = matrix(0, nrow = 1e3*5, ncol = 10)
    lln$runningMean = matrix(0, nrow = 1e3*5, ncol = 10)
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
          The algorithm works by simulating a Markov chain whose stationary distribution is the target distribution, i.e. eventually the samples from the Markov chain will look similar to samples from the target.
          The selected Gaussian MH algorithm has transition kernel N(x,", h(), "), where x is the current value of the chain."
        )
      } else if (kernel() == "mh_indep") {
        paste("Our aim is to produce samples from our selected target distribution. We use the Metropolis-Hastings algorithm to accomplish this task.
          The algorithm works by simulating a Markov chain whose stationary distribution is the target distribution, i.e. eventually the samples from the Markov chain will look similar to samples from the target.\n
          The selected Independent MH algorithm has transition kernel N(2,", h(), ")."
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
          ylab('Density') + xlab("N = 1e4") +
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
      if(time() == 0){
        plots$target
      } else {
        plots$mh_anime_stat[[time()]]
      }
    }
  })

  output$time_static_stat = renderPlot({
    if(control$computed){
      if(time() == 0){
        plots$target
      } else {
        plots$mh_static_stat[[time()]]
      }
    }
  })

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

  output$slln = renderPlot({
    if(control$computed){
      N = 1e3*5
      reps = 10
      df = data.frame(data = lln$runningMean, draws = 1:N)
      for(i in 1:reps){
        colnames(df)[i] = paste("Chain", i)
      }
      df1 = melt(df, id.vars = 'draws', variable.name = 'Chains')
      ggplot(df1, mapping = aes(draws, value)) +
        geom_line(aes(color = Chains)) +
        geom_line(mapping = aes(y = lln$mean), lty = 2, size = 1) +
        theme_classic() +
        theme(legend.position = 'none')
    }
  })
}

shinyApp(ui, server)
