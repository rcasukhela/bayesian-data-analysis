library("rstanarm")
library("loo")
library("bayesplot")
rm(list=ls())
dev.off()
dev.off()

setwd("C:/Users/rcasu/OneDrive - The Ohio State University/OSU/demos/bayesian-data-analysis")
dat = read.table("regstories/ROS-Examples/Mesquite/data/mesquite.dat", header = TRUE)

# Data were collected in order to develop a method of estimating the total biomass production
# of mesquite leaves using easily measured parameters of the plant, before actual harvesting
# takes place.

# Two separate sets of measurements were taken, one on a group of 26 mesquite bushes
# and the other on a different group of 20 mesquite bushes measured at a different time of year.
# All the data were obtained on the same ranch, but neither constituted a strictly
# random sample.

##########################################################################################################

# Response: weight        : biomass in grams.

# Input 1: diam1          : diameter of canopy (leafy area) in meters, measured along longer axis of bush.
# Input 2: diam2          : canopy diameter measured along the shorter axis.
# Input 3: canopy_height  : height of the canopy.
# Input 4: total_height   : total height of the bush.
# Input 5: density        : plant unit density (# of primary stems per plant unit)
# Input 6: group          : group of measurements (0 for the first group, 1 for the second group)

##########################################################################################################

# It is reasonable to predict the leaf weight using some sort of regression model.
# Many formulations are possible. The simplest approach is to regress `weight`
# on all of the predictors:

fit_1 = stan_glm(formula = weight ~ 
                   diam1 + diam2 + canopy_height + total_height + density + group,
                 data = dat)

fit_1

# Wow, there is a lot of noise here. All standard errors are very high, and the RSS is very high as well.
# However, this is also because there is such a huge range in the response, right?
# We should expect to find 68% of all predicted values within 270 grams of the predictor line.

max(dat$weight) - min(dat$weight) # <-- total difference of 3991.8 grams! So the RSS makes sense.

# This is definitely true. However, the coefficient SEs are still very high. I suspect that this
# is because of multicollinearity, and unsurprisingly:

cor(dat$diam1, dat$diam2) # <-- correlation is 0.89. Very high correlations.

library(reshape2)
corr_mat <- round(cor(dat[, c(3:7)]),2)
corr_mat # <-- yep, as suspected. So some variable selection will have to be done before proceeding.

# The book suggests fitting a LOOCV.

loo_1 = loo(fit_1) 

# However, the LOO computation is unstable, and K-fold CV is recommended.
# The p_loo, the effective number of parameters, is larger than the total number of parameters too,
# so this indicates that model assumptions are not met with the data.

# We get warnings about high Pareto k values, which indicates that the importance sampling
# approximation used in loo is in this case unreliable. We thus use more robust K-fold-CV.

kfold_1 = kfold(fit_1, K=10)

# We should never interpret the expected log posterior density score outright, but here,
# we notice that the 10-fold and LOO CV methods produce similar ELPDs, which suggest that they
# both give accurate estimates of the ELPD. Furthermore, p, the effective number of parameters,
# is quite high again for the 10-fold CV. This tells us that our LOOCV was correctly indicating
# problems.

# Standardization of predictors can be useful to remove the effect of large variance and covariance
# from the model, thereby allowing you properly compare coefficients with each other.

# Compare your CV R^2 values to your total model R^2 values. If total model R2 > CV R2, you can
# safely deduce overfitting.

log_dat = data.frame(apply(dat[,3:ncol(dat)], 2, log))

fit_2 = stan_glm(formula = weight ~ ., data=log_dat)

loo_2 = loo(fit_2)

# To compare fit_1 and fit_2, you need to correct for the transformed input variable.

loo_2_with_jacobian = loo_2
loo_2_with_jacobian$pointwise[,"elpd_loo"] = 
  loo_2_with_jacobian$pointwise[,"elpd_loo"] - log(mesquite$weight)

# Compute adjusted LOO
sum(loo_2_with_jacobian$pointwise[,"elpd_loo"])

# Comparison of two models
loo_compare(kfold_1, loo_2_with_jacobian)

# So, fit_2 does better overall, because of the higher, more positive, log score.






























