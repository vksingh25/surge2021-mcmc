# Target distribution (exponential)
target <- function(x) {
  return (ifelse(x < 0, 0, exp(-x)))
}

N <- 1e4  # Number of draws
x <- rep(0, N)
x[1] <- 3  # Initial value
for(i in 2:N){
  current_x <- x[i-1]
  proposed_x <- current_x + rnorm(1, 0, 1)  # Proposed value
  A <- min(1, target(proposed_x)/target(current_x))  # MH Acceptance rate
  if(runif(1) < A) {
    x[i] <- proposed_x
  } else {
    x[i] <- current_x
  }
}

par(mfrow = c(1,1))
# Plot 1
plot(density(x), col = "blue", xlim = c(0,10), main = "Density plot")
lines(density(rexp(1e5)), col = "red")
legend("topright", col = c("red", "blue"), legend = c("target", "current"),
       lty = 1)
# Plot 2
acf(x, main = "ACF Plot")
# Plot 3
plot.ts(x, main = "Trace plot")

