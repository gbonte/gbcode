library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
Y<-rnorm(10,1.5,0.2)
fit<-stan('simple.stan',iter=20,chains=4,data=list(Y=Y))

library(ggplot2)
mu<-extract(fit,'mu')[[1]]
qplot(mu)

library(shinystan)
aFit<-as.shinystan(fit)
launch_shinystan(aFit)