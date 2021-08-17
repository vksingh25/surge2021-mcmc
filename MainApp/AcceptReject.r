# # TODO: Need to add proposal curves along with points.

# #	blue the i-1 dot and keep the i-1 plot grey,
# #first plot the prop dot and curve (dotted), show the alpha,
# #if accepted, make the curve green (now solid) and dot green as well,
# #else go back to the previous curve and make the dot red
library(ggplot2)
set.seed(1)

chisq_den = function(x, k){
	rtn = 0
	if (x > 0){
		rtn = x^(k/2-1) * exp(-x/2)
	}
	return(rtn)
}

N = 5
h = 100
k = 10
# k =  degrees of freedom
# h = proposal variance N(x, h)
# start = starting value
plots_ar = list() # to store accept reject phase
plots_inter = list() # to store intermediate phase
plots = list() # to store all time steps
samp = numeric(length = N)
prop = numeric(length = N)
acc = numeric(length = N)
target = rchisq(1e4, df = k)

acc[1] = 1
samp[1] = 3
prop[1] = 3
for(t in 2:N){
	prop[t] = rnorm(1, mean = 2, sd = 8)
	alpha = chisq_den(x = prop[t], k = k) / chisq_den(x = samp[t-1], k = k)
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
### For testing
t = 5
p = ggplot(data = data.frame(x = prop[1:t-1], y = numeric(length = t-1)), mapping = aes(x = x, y = y)) +
	geom_point(aes(colour = colors[1:t-1])) +
	scale_color_manual(name = "Legend", values = c('blue' = 'blue', 'red' = 'red'), labels = c('red' = 'reject', 'blue' = 'accept')) +
	coord_cartesian(xlim = c(-20, 50), ylim = c(0, 0.2)) +
  theme_classic()
p1 = p + geom_point(mapping = aes(x = prop[t], y = 0), colour = 'gray')
color_curr = ifelse(acc[t], "green", "red")
p2 = p + geom_point(mapping = aes(x = prop[t], y = 0), colour = color_curr)
p1
p2
###------------------

plots_ar[[1]] = ggplot(data = data.frame(x = prop[1], y = 0), mapping = aes(x = x, y = y)) +
	geom_point(colour = "green") +
	coord_cartesian(xlim = c(-20, 50), ylim = c(0, 0.2)) +
  theme_classic()
plots[[1]] = plots_ar[[1]]
counter = 2
for(i in 2:N){
	p = ggplot(data = data.frame(x = prop[1:i-1], y = numeric(length = i-1)), mapping = aes(x = x, y = y)) +
		geom_point(aes(colour = colors[1:i-1])) +
		scale_color_manual(name = "Legend", values = c('blue' = 'blue', 'red' = 'red'), labels = c('red' = 'reject', 'blue' = 'accept')) +
		coord_cartesian(xlim = c(-20, 50), ylim = c(0, 0.2)) +
		theme_classic()
	plots[[counter]] = p + geom_point(mapping = aes(x = prop[i], y = 0), colour = 'grey')
	plots[[counter]] = plots_inter[[i]]
	counter = counter + 1
	color_curr = ifelse(acc[i], 'green', 'red')
	plots[[counter]] = p + geom_point(mapping = aes(x = prop[i], y = 0), colour = color_curr)
	plots_ar[[i]] = plots[[counter]]
	counter = counter + 1
}