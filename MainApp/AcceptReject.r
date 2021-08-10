#

set.seed(1)

chisq_den = function(x, k){
	rtn = 0
	if (x > 0){
		rtn = x^(k/2-1) * exp(-x/2)
	}
	return(rtn)
}

reps = 10
h = 100
# k =  degrees of freedom
# h = proposal variance N(x, h)
# start = starting value
out = numeric(length = reps)
gaussian_mh = function(reps = 10, k = 10, h = 100, start = 3){
	out[1] = start
	for(i in 2:reps){
		prop = rnorm(1, mean = out[i-1], sd = sqrt(h))
		alpha = chisq_den(prop, k)/chisq_den(out[i-1], k)
		U = runif(1)
		if(U <= alpha){
			out[i] = prop
		} else {
			out[i] = out[i-1]
		}
	}
	return (out)
}
