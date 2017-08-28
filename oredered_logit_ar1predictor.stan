data {
      int<lower=2> K;
      int<lower=0> N;
      int<lower=1> D;
      int<lower=1,upper=K> y[N];
      row_vector[D] x[N];
      real<lower=0, upper=1> g_init; // fat prop initial level
    }
    parameters {
      real beta;
      real<lower=0> sigma;
      ordered[K-1] c;
      vector[N] g;
    }
model {
      for (n in 1:N)
        y[n] ~ ordered_logistic(g[n] * beta, c);
    
      g[1] ~ normal(g_init, sigma);
    
      for (n in 2:N) 
        g[n] ~ normal(g[n-1] + x[n], sigma);
}
