start <- Sys.time()

# Target distribution (exponential)
target <- function(x) {
  return (ifelse(x < 0, 0, exp(-x)))
}

N <- 1e6  # Number of draws
normals <- rnorm(N, 0, 1)
uniforms <- runif(N)
x <- rep(0, N)
x[1] <- 3  # Initial value
for(i in 2:N){
  current_x <- x[i-1]
  proposed_x <- current_x + normals[i]  # Proposed value
  A <- min(1, target(proposed_x)/target(current_x))  # MH Acceptance rate
  if(uniforms[i] < A) {
    x[i] <- proposed_x
  } else {
    x[i] <- current_x
  }
}

end <- Sys.time()
# To measure the runtime of the code
total_time <- as.numeric (end - start, units = "secs")
# "[1] 4.452415" as compared to "[1] 9.357162" without any vectorisation for N = 1e6


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
