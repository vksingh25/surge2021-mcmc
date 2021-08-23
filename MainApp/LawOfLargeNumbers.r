library(ggplot2)
library(reshape2)
set.seed(1)

chisq_den = function(x, k){
  rtn = 0
  if (x > 0){
    rtn = x^(k/2-1) * exp(-x/2)
  }
  return(rtn)
}

chisq_mh  <- function(N = 1e3, k, h, start)
{
  # memory allocation
  out <- numeric(length = N)
  acc.prob <- 0  # acceptance probability
  out[1] <- start  # just some starting value (fixed starting value)

  for(t in 2:N)
  {
    # proposal N(x, h). Use sd = sqrt(variance) in R
    prop <- rnorm(1, mean = out[t-1], sd = 8^2)

    # the proposal density gets cancelled here
    alpha <- chisq_den(x = prop, k = k) / chisq_den(x = out[t-1], k = k)

    U <- runif(1)
    if(U <= alpha)  # to decide whether to accept or reject
    {
      out[t] <- prop
      acc.prob <- acc.prob + 1
    } else{
      out[t] <- out[t-1]
    }
  }
  print(acc.prob/N)   # we want to see often we accept
  return(out)
}

N = 1e3*5 # Number of steps
reps = 1e1 # Number of independent chains
k = 10 # df of chisq
h = 1000

chain.fixed = matrix(0, nrow = N, ncol = reps)
for(r in 1:reps){
  chain.fixed[, r] = chisq_mh(N = N, k = k, h = h, start = 3)
}

running.mean = matrix(0, nrow = N, ncol = reps)
for(i in 1:N){
  for(j in 1:reps){
    running.mean[i, j] = mean(chain.fixed[1:i, j])
  }
}

print(chain.fixed[90:100, 1])
print(running.mean[900:1000, 1] - running.mean[900:1000, 4])
df = data.frame(data = running.mean, draws = 1:N)
for(i in 1:reps){
  colnames(df)[i] = paste("Chain", i)
}
colnames(df)
df1 = melt(df, id.vars = 'draws', variable.name = 'Chains')
ggplot(df1, mapping = aes(draws, value)) +
  geom_line(aes(color = Chains)) +
  geom_line(mapping = aes(y = 10), lty = 2, size = 1) +
  theme_classic() +
  theme(legend.position = 'none')
