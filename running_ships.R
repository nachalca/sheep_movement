
# Testing ordered model for ship movement models
library(tidyverse)
library(ggplot2)
library(rstan)


# 1) An oredered_probit model
# simulate a small data to test ordered_probit model
b <- 3.5
K <- 5

dd <- data_frame(x = rnorm(1e2, sd=2), 
                 y = cut_number(x*b+rnorm(1e2), K, levels=1:K, labels=1:K) %>% as.integer() 
)
ggplot(dd) + geom_point(aes(x,y))
ggplot(dd) + geom_histogram(aes(x=y))

md_ordprob <- stan_model(file = 'Rcode/ordered_probit.stan')

res <- sampling(md_ordprob, data = list(K=K, N=nrow(dd), D=1, y = dd$y, x = as.matrix(dd$x),ncol=1) )

#########################
# 2) Combine Order probit (or logit) with an AR(1) 
#########################
# c_t: obs fat index (1-17), g_t: fat proportion (unobs), E_t: 'energy' balance spent during day 
# Model: 
# c_t ~ ordered_logit( g_t)
# g_t = g_(t-1) + E_t + eps_t; eps_t ~ normal(0, sigma)

#asume : E_t known
#########################
b <- .7
K <- 5

dd <- data_frame(time=1:1e2, eps = rnorm(1e2), 
                 E = rnorm(1e2, 4), gdif = rnorm(1e2, sd=2)
) %>% mutate(
  g_obs = diffinv(gdif)[-1], 
  y = cut_number(g_obs*b + rnorm(1e2), K, levels=1:K, labels=1:K) %>% as.integer() 
)

ggplot(dd) + geom_point(aes(time,g_obs, color=as.factor(y)))


md_ordAR <- stan_model(file = 'Rcode/oredered_logit_ar1predictor.stan')

res <- sampling(md_ordAR, 
                data = list(K=K, N=nrow(dd), D=1, y = dd$y, x = as.matrix(dd$E,ncol=1), g_init=1 )
                )





