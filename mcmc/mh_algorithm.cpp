#include <Rcpp.h>
using namespace Rcpp;

float target_density(float x, String dist, List parameters){
	float rtn = 0;
    if(dist == "chisq") {
      float k = parameters["df_chisq"];
      rtn = R::dchisq(x, k, false);
    } else if (dist == "norm") {
      float mean = parameters["mean_norm"];
      float sd = parameters["sd_norm"];
	  rtn = R::dnorm(x, mean, sd, false);
    } else if (dist == "t.dist") {
      float k = parameters["df_t"];
      rtn = R::dt(x, k, false);
    }
    return (rtn);
}

// [[Rcpp::export]]
NumericVector mh_loop(long N, String kernel, String dist, List parameters, float prop_sd, float start, NumericVector U) {
	NumericVector out(N);
	out[0] = start;
	float mean = 2;
	for(int i = 1; i < N; ++i){
		if(kernel == "mh_dep"){
			mean = out[i-1];
		}
		float prop = R::rnorm(mean, prop_sd);
		float alpha = (target_density(prop, dist, parameters)/target_density(out[i-1], dist, parameters));
		if(kernel == "mh_indep"){
			alpha = alpha * (R::dnorm(out[i-1], mean, prop_sd, false)/R::dnorm(prop, mean, prop_sd, false));
		}
		if(U[i-1] <= alpha) {
			out[i] = prop;
		} else {
			out[i] = out[i-1];
		}
	}
	return (out);
}
