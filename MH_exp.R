library(ggplot2)
library(ggpubr)

# Target distribution (exponential)
target <- function(x) {
  return (dexp(x))
}

N <- 10000  # Number of draws
x <- rep(0, N)
acceptance <- rep(0, N)
x[1] <- 3  # Initial value
acceptance[1] = 1;
for(i in 2:N){
  current_x <- x[i-1]
  proposed_x <- current_x + rnorm(1, 0, .6)  # Proposed value
  A <- min(1, target(proposed_x)/target(current_x))  # MH Acceptance rate
  if(runif(1) < A) {
    x[i] <- proposed_x
    acceptance[i] = 1;
  } else {
    x[i] <- current_x
  }
}
output <- data.frame(x, y = 1:N)
target <- data.frame(target = rexp(1e4))

# Plot 1 : Density plot
density.plot <- ggplot(data = output, mapping = aes(x = x, color = 'blue', linetype = 'current')) +
  geom_line(stat = 'density') +
  geom_line(data = target, mapping = aes(x = target, color = 'red', linetype = 'target'), stat = 'density', lty = 2) +
  scale_color_manual(name = 'Legend', values = c('blue' = 'blue', 'red' = 'red'), labels = c('current', 'target')) +
  scale_linetype_manual(name = 'Legend', values = c('current' = 1, 'target' = 2)) +
  ylab('Density') + xlab(paste("N = ", N, ", Acceptance Rate = ", mean(acceptance))) +
  labs(title = "Density Plot")

# Plot 2 : ACF Plot
p <- acf(x, plot = FALSE)
baseACF <- with(p, data.frame(lag, acf))
conf.level <- 0.95
ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
acf.plot <- ggplot(data = baseACF, mapping = aes(lag, acf)) +
  geom_hline(yintercept = 0) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  geom_hline(yintercept = ciline, color = "blue", lty = 2) +
  geom_hline(yintercept = -ciline, color = "blue", lty = 2) +
  xlab("Lag") + ylab("ACF") +
  labs(title = "ACF Plot")

# Plot 3 : Trace Plot
ts.plot <- ggplot(data = output, mapping = aes(y, x)) +
  geom_line() +
  xlab("Time") + ylab("Proposal") +
  labs(title = "Trace Plot")


ggarrange(density.plot,
          ggarrange(acf.plot, ts.plot, ncol = 2),
          nrow = 2)





