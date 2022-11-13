library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyjs)
library(reshape2)

library(Rcpp)
sourceCpp('mh_algorithm.cpp')

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
    "kernel", tags$h4(tags$b("Proposal Distribution")),
    c(
      "Random Walk - N(x_t, h)" = "mh_dep",
      "Inde MH - N(0, h)" = "mh_indep"
    )
  ),
  numericInput("h", tags$h4(tags$b("Step Size (h)")), value = 10, min = 0.05, max = 100),
  selectInput(
    "starting_dist", tags$h4(tags$b("Starting Distribution")),
    c(
      "Fixed at 3" = "fixed",
      "Exp(0.05)" = "exp1",
      "Exp(0.01)" = "exp2"
    )
  ),
  p("Please wait ~10 secs after pressing Start for the animations to be ready."),
  # uiOutput("startButton"),
  actionButton(inputId = 'start', label = 'Start', width = 220),
  actionButton(inputId = 'reset', label = 'Reset', width = 220),
  p("For a new simulation, click Reset."),
  sidebarMenu(
    menuItem("Shiny app developed by:", startExpanded = TRUE,
      menuSubItem("Vivek Kumar Singh", tabName = "subItem1")
    ),
    menuItem("Under the guidance of:", startExpanded = TRUE,
      menuSubItem(text = paste("Prof. Dootika Vats"), href = "https://dvats.github.io")
    )
  )
)
body = dashboardBody(
  fluidPage(
    box(
      title = 'Convergences in Markov chain Monte Carlo', width = NULL, status = 'primary', solidHeader = TRUE,
      # Everything about the app, how to use it, etc
      uiOutput("about_app"),
      tags$head(tags$style("#about_app{
                             font-size: 16px;
                            }"
      )
      )
    ),
    box(
      width = NULL, status = 'warning',
      "NOTE: You can switch between the static and animation tabs to see respective demonstrations of the said concepts.
        The blue triangle is the play button. Click that to play the animations. You can also use the slider to skip or rewind steps if you wish."
    ),
    box(
      title = 'About Metropolis Hastings algorithm', width = NULL, status = 'primary', solidHeader = TRUE,
      # Everything about the app, how to use it, etc
      uiOutput("algo_desc"),
      tags$head(tags$style("#algo_desc{
                             font-size: 16px;
                            }"
      )
      )
    ),
    # box(
    #   width = NULL, status = 'warning',
    #   "Using the chosen settings, 1000 independent chains are run, each of length 5000. The multiple chains is only for demonstrating the distributional behavior of the Markov chains.", br(), "", br(),
    #   "Static tab: estimated density from the samples compared with the chosen target.", br(),
    #   "Animation tab: The accept-reject procedure for 20 draws."
    # ),
    fluidRow(
      column(
        width = 9,
        tabBox(
          title = "MH Plot", id = "tabset_app1", width = NULL,
          tabPanel(title = "Static", plotOutput("mh_density"), value = 1, solidHeader = TRUE),
          tabPanel(title = "Animation", plotOutput("mh_density_anime"), value = 2)
        )
      ),
      column(
        width = 3,
        box(
          title = "About Target", width = NULL, solidHeader = TRUE,
          textOutput("aboutTarget"),
          tags$head(tags$style("#aboutTarget{
                             font-size: 16px;
                            }"
          )
          )
        ),
        box(
          title = "Slider", width = NULL, solidHeader = TRUE,
          sliderInput(inputId = "targetAnimation", label = "Animation", min = 1, max = 19, value = 1, animate = animationOptions(interval = 1500)),
          tags$head(tags$style(
            type='text/css',
            ".slider-animate-button {
                font-size: 50pt !important;
              }
              .slider-animate-container {
                text-align: center;
                margin-top: 0px !important;
                margin-bottom: -10px;
              } "
          )
          ),
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          title = "ACF Plot", width = NULL, solidHeader = TRUE,
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
          title = "Trace Plot", width = NULL, solidHeader = TRUE,
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
      title = 'About Time Evolution', width = NULL, status = 'primary', solidHeader = TRUE,
      # Everything about selected algorithm
      uiOutput("timeDesc"),
      tags$head(tags$style("#timeDesc{
                             font-size: 16px;
                            }"
      )
      )
    ),

    box(
      title = "About Stationarity", width = NULL, status = 'primary', solidHeader = TRUE,
      uiOutput("aboutStationarity"),
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
          title = "Stationarity", id = "tabset_app2.1", width = NULL,
          tabPanel(title = "Animation", plotOutput("time_anime_stat"), value = 1),
          tabPanel(title = "Static", plotOutput("time_static_stat"), value = 2)
        )
      ),
      column(
        width = 3,
        box(
          title = "Slider", width = NULL, solidHeader = TRUE,
          sliderInput(inputId = "time_stat", label = "Number of Draws", min = 0, max = 100, value = 0, animate = animationOptions(interval = 750)),
          tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }"))
        )
      )
    ),
    box(
      title = "About Ergodicity", width = NULL, status = 'primary', solidHeader = TRUE,
      uiOutput("aboutErgodicity"),
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
          title = "Slider", width = NULL, solidHeader = TRUE,
          sliderInput(inputId = "time_erg", label = "Number of Draws", min = 0, max = 100, value = 0, animate = animationOptions(interval = 750)),
          tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }"))
        )
      )
    ),
    box(
      title = 'About Strong Law of Large Numbers', width = NULL, status = 'primary', solidHeader = TRUE,
      uiOutput("aboutSLLN"),
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
      title = 'Central Limit Theorem in Markov chains', width = NULL, status = 'primary', solidHeader = TRUE,
      uiOutput("aboutCLT"),
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
  dashboardHeader(titleWidth = 250, title = "Markov chain Monte Carlo"),
  sidebar,
  body
)

server = function(input, output, server) {

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
  size_lln.clt = 5000

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
  lln.clt$runningMean = matrix(0, nrow = size_lln.clt, ncol = reps_lln)
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
      rtn = dchisq(x, df = k)
    } else if (dist == 'norm') {
      mean = parameters$mean_norm
      sd = parameters$sd_norm
      rtn = dnorm(x, mean = mean, sd = sd)
    } else if (dist == 't.dist') {
      k = parameters$df_t
      rtn = dt(x, df = k)
    }
    return (rtn)
  }

  # returns proposal values using selected algorithm
  target_mh = function(N, start = 3, kernel, dist, h, parameters){
    out = numeric(length = N)
    U = runif(N-1)
    out[1:N] = mh_loop(N, kernel, dist, parameters, sqrt(h), start, U)
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
    for(t in 2:N_anime){
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
    p_base = ggplot(data = data.frame(target = target_curve), mapping = aes(x = target)) +
          geom_line(stat = 'density', lty = 2) +
          geom_point(data = data.frame(x = prop[1:N_anime], y = numeric(length = N_anime)), mapping = aes(x = x, y = y), colour = "white", size = 0.01)
    xlim = local({
      ggplot_build(p_base)$layout$panel_scales_x[[1]]$range$range
    })
    ylim = local({
      ggplot_build(p_base)$layout$panel_scales_y[[1]]$range$range
    })
    p_base = p_base + coord_cartesian(xlim = xlim, ylim = ylim)
    plots = list()
    plots[[1]] = p_base +
      geom_point(data = data.frame(x = prop[1], y = numeric(length = 1)), mapping = aes(x = x, y = y), colour = 'green', size = 3) +
      scale_color_manual(name = "Legend", values = c('blue' = 'blue', 'red' = 'red'), labels = c('red' = 'reject', 'blue' = 'accept')) +
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
        p_inter = p_base +
          geom_point(data = data.frame(x = prop[1:t-1], y = numeric(length = t-1)), mapping = aes(x = x, y = y), colour = colors[1:t-1]) +
          scale_color_manual(name = "Legend", values = c('blue' = 'blue', 'red' = 'red'), labels = c('red' = 'reject', 'blue' = 'accept')) +
          theme_classic() +
          geom_line(data = data.frame(target = prop_curve), mapping = aes(x = target), stat = 'density', color = 'grey') +
          geom_point(mapping = aes(x = prop[t], y = 0), colour = 'grey', size = 3)
      })
      color_curr = ifelse(acc[i], 'green', 'red')
      plots[[counter + 1]] = local({
        t = i
        p_ar = p_base +
          geom_point(data = data.frame(x = prop[1:t-1], y = numeric(length = i-1)), mapping = aes(x = x, y = y), colour = colors[1:t-1]) +
          scale_color_manual(name = "Legend", values = c('blue' = 'blue', 'red' = 'red'), labels = c('red' = 'reject', 'blue' = 'accept')) +
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

  targetPlot.dist = function(target){
    p = ggplot(data = data.frame(target = target), mapping = aes(x = target)) +
      geom_line(stat = 'density', linetype = 'dashed', lwd = 0.75) +
      theme_classic()
    xlim = local({
      ggplot_build(p)$layout$panel_scales_x[[1]]$range$range
    })
    ylim = local({
      ggplot_build(p)$layout$panel_scales_y[[1]]$range$range
    })
    p = p + coord_cartesian(xlim = xlim, ylim = ylim)
    return (p)
  }

  density.plots = function(N, start, kernel, dist, h, parameters) {
    print("time evolution density plots")
    for(r in 1:reps_chain){
      chain$values[r, ] = target_mh(N = N, start = start, kernel, dist, h, parameters)
      chain$values_stat[r, ] = target_mh(N = N, start = random.dist(1, dist, parameters), kernel, dist, h, parameters)
    }
    chain$target = random.dist(1e5, dist, parameters)
    plots$target = targetPlot.dist(chain$target)
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
    print("draw Independent chains")
    chains = matrix(0, nrow = N, ncol = reps)
    for(r in 1:reps){
      chains[, r] = target_mh(N, start, kernel, dist, h, parameters)
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
      Time = Sys.time()
      print("Simulation Started!!")
      density$proposal = target_mh(N = 1e4, start = starting.draw(starting_dist()), kernel(), dist(), h(), parameters())
      density.plots(N = N_chain, start = starting.draw(starting_dist()), kernel(), dist(), h(), parameters())
      control$computed = 1
      density$plots = acceptReject_mh(N_anime = 20, start = starting.draw(starting_dist()), kernel(), dist(), h(), parameters())
      lln.clt$mean = calculateMeanDistribution(dist(), parameters())
      lln.clt$values = drawIndependentChains(N = size_lln.clt, reps = reps_clt, start = starting.draw(starting_dist()), kernel(), dist(), h(), parameters())
      lln.clt$runningMean = calculateRunningMean(N = size_lln.clt, reps = reps_lln, chains = lln.clt$values[, 1:reps_lln])
      print("Simulation Complete!!")
      print(Sys.time()-Time)
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
    updateSliderInput(inputId = "targetAnimation", value = 1)
    updateSliderInput(inputId = "time_stat", value = 0)
    updateSliderInput(inputId = "time_erg", value = 0)
  })

  # output plots of app 1
  output$about_app = renderUI({
    withMathJax(HTML(paste0("Markov chain Monte Carlo (MCMC) is a technique that is used to draw samples from complicated probability distributions using a Markov chain.", br()," ",br(), "
      Our applet tries to visually depict the following important theoretical characteristics of an MCMC problem.", br(),br(), "* The Metropolis-Hastings Algorithm", br(), "* Stationarity", br(), "* Ergodicity", br(), "* Law of large numbers", br(), "* Central limit theorem")))
    # withMathJax(HTML(paste0("Use this formula: $$\\hat{A}_{\\small{\\textrm{M€}}} =", 1,"$$ abcdefg")))
  })

  output$algo_desc = renderUI({
      withMathJax(HTML(paste0("$$ \\pi \\text{ is the target distribution, }$$ $$ Q \\text{ is the proposal distribution} $$ $$x_t \\text{ is the current value of the chain.}$$ Our aim is to produce samples from the selected target distribution. We use the Metropolis-Hastings algorithm to accomplish this task. ", br(), " ", br(),"
          This algorithm proposes a value from the chosen proposal distribution and accepts or rejects it with a certain probability.  Below are the 3 steps of the MH algorithm.", br(), " ", br(), 
          
          "STEP 1: a value y is proposed from the proposal distribution Q ", br(),"
          STEP 2: MH ratio alpha is calculated using the proposal and the current value of the Markov chain:
          $$\\alpha = \\text{min}\\left(1, \\frac{\\pi(y)Q(x_t|y)}{\\pi(x_t)Q(y|x_t)} \\right).$$
          STEP 3: the proposal is selected with probability alpha. If rejected, then the new value of the chain is same as the current value.")))
  })

  output$mh_density = renderPlot({
    if(control$computed){
      ggplot(data = data.frame(output = density$proposal), mapping = aes(x = output, color = 'blue', linetype = 'current')) +
        geom_line(stat = 'density') +
        geom_line(data = data.frame(target = chain$target), mapping = aes(x = target, color = 'black', linetype = 'target'), stat = 'density', lty = 2) +
        scale_color_manual(name = 'Legend', values = c('blue' = 'blue', 'black' = 'black'), labels = c('current', 'target')) +
        scale_linetype_manual(name = 'Legend', values = c('current' = 1, 'target' = 2)) +
        ylab('Density') + xlab(paste("N =", reps_chain)) +
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
      paste("Chi-squared distribution with", parameters()$df_chisq, "degrees of freedom")
    } else if (dist() == 'norm') {
      paste("Normal distribution with mean", parameters()$mean_norm, "and standard deviation", parameters()$sd_norm)
    } else if (dist() == 't.dist') {
      paste("t-distribution with", parameters()$df_chisq, "degrees of freedom")
    }
  })

  output$aboutACF = renderText({
    paste("Autocorrelation function tells us how correlated the draws are as the time gap (lag) between them increases.  The larger the value of lag, for which the vertical lines are significantly different from 0, the more correlated the process is.")
  })

  output$mh_acf = renderPlot({
    if(control$computed){
      acf(density$proposal, main = "")
    }
  })

  output$aboutTrace = renderText({
    paste("Trace plot plots the actual draw values versus time index.
      This tells us how well the process is moving in the space.")
  })

  output$mh_trace = renderPlot({
    if(control$computed){
      ggplot(data = data.frame(output = density$proposal, time = 1:reps_chain), mapping = aes(x = time, y = output)) +
        geom_line() +
        xlab("Time") + ylab("Proposal") +
        theme_classic()
    }
  })

  # output plots for app 2
  output$timeDesc = renderUI({
    HTML(paste0("Every iterate of the Markov chain has a marginal distribution. The density plots aim to compare the marginal distribution at increasing times versus the target distribution.", br(), " ", br(), "
      We do two different kinds of runs:", br(), " ", br(),
      "(i) we start the Markov chain from the target distribution (this is usually not possible to do, and we do it for demonstration of an ideal situation)", br(), 
      "(ii) we start the Markov chain from the chosen starting distribution", br(), " ", br(), 
      "In both cases, we sample 1000 independent Markov chains from the selected starting distribution and run each of them for 100 iterations.
      Then we plot the marginal density plot of tth time step of the chain using the 1000 different replications. ", br(),  " ", br(), "
      In both the time evolution plots following this, the first tab shows the animation of how the marginal density evolves with time. ", br(), "
      The second tab shows all the densities from the start till time k for better visualization of the convergence."
    ))
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

  output$aboutStationarity = renderUI({
    HTML(paste("A Markov chain is said to be stationary if every iterate X_n of the Markov chain has the same distribution as the target distribution.
      This is possible when the intial draw is from the target distribution itself. All iterates of the chain will be identically distributed but not independent (as seen in the ACF plot). Stationarity can be seen in the plot below, where we plot the estimated density of the 1000 independent Markov chains for each time point."))
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

  output$aboutErgodicity = renderUI({
    HTML(paste("A Markov chain is said to be ergodic if the marginal distribution of X_n converges to the target distribution, irrespective of the starting distribution. We observe ergodicity in the plot below, where the estimated density at each time keeps getting closer to the target distribution."))
  })

  output$aboutSLLN = renderUI({
    withMathJax(HTML(paste0("$$\\bar{X}_n:= \\dfrac{1}{n} \\sum_{t=1}^{n} X_t \\overset{a.s.}{\\to} E_{\\pi}(X) < \\infty$$", "The Strong Law of Large Numbers for a Markov chain holds when the chain is ergodic. Which means that irrespective of the starting value, sample averages will converge to expectations under the target distribution (assuming it is finite).
               For an example where SLLN doesn't hold, set target to t-distribution and df ≤ 1; here the mean is undefined")))
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
        theme(legend.position = 'none') +
        labs(title = "Running mean plot")
    }
  })

  output$aboutCLT = renderUI({
    withMathJax(HTML(paste0("$$\\sqrt{n} \\left(\\bar{X}_n - E_{\\pi}(X) \\right) \\overset{d}{\\to} N(0, \\sigma^2)  \\quad\\text{ where}$$", 
    "$$ \\sigma^2 = Var_{\\pi}(X) + 2 \\sum_{k=1}^{\\infty}Cov_{\\pi}(X_1, X_{1+k}) $$", 
    "The Central Limit Theorem  (above), when it holds, states that the scaled Monte Carlo error converges to a Gaussian distribution, with variance that accounts for the serial correlation in the chain. 
    In the plot below, we calculate the left-hand-side for the 1000 independent chains and make a histogram. You can see that the histogram mimics a Normal distribution entered at 0 with some variance.")))
  })

  output$clt = renderPlot({
    if(control$computed){
      N = size_lln.clt
      reps = reps_clt
      means = colMeans(lln.clt$values)
      cltError = data.frame(mean = sqrt(reps)*(means - lln.clt$mean))
      print((sapply(cltError, max)-sapply(cltError, min)))
      ggplot(data = cltError, aes(x = mean)) +
        geom_histogram(aes(y = ..density..), binwidth = (sapply(cltError, max)-sapply(cltError, min))/50, alpha = .9, color = "#63BCC9", fill = "#B5EAD7", size = 0.4) +
        geom_density(alpha = 1, size = 1) +
        geom_vline(aes(xintercept = 0), lty = 2, size = 1) +
        theme_classic()
    }
  })
}

shinyApp(ui, server)
