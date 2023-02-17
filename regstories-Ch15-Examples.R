rm(list=ls())
setwd("C:/Users/rcasu/OneDrive - The Ohio State University/OSU/demos/bayesian-data-analysis/")

library("rstanarm")
library("bayesplot")

roaches = read.csv('data/ros_ch15_roaches.csv', header=TRUE)

roaches$roach100 = roaches$roach1/100
roaches$exposure = roaches$exposure2

fit_1 = stan_glm(y ~ roach100 + treatment + senior,
                 family=neg_binomial_2,
                 offset=log(exposure),
                 data=roaches
                 )
sims = as.matrix(fit_1)

print(fit_1, digits=2)

y_rep_1 = posterior_predict(fit_1) # in this case, each row is a new dataset.

n_sims = nrow(y_rep_1)
subset = sample(n_sims, 100)
ppc_dens_overlay(log10(roaches$y+1), log10(y_rep_1[subset,]+1))


# One might be concerned that the number of zeroes in the data is not what would be predicted
# by the model. To check this formally, we can define a test statistic and compute it for each
# of the replications separately:

test = function(y){
  mean(y==0)
}
test_rep_1 = apply(y_rep_1, 1, test)
























