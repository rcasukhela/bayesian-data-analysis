rm(list=ls())
dev.off()
dev.off()
library("rstanarm")

setwd("C:/Users/rcasu/OneDrive - The Ohio State University/OSU/demos/bayesian-data-analysis")
dat = read.csv("data/ros_ch7_electionseconomy.txt", header = TRUE, sep = " ")

x = dat[, !(names(dat) %in% c("vote", "inc_party_candidate", "other_candidate"))]
y = dat[,  (names(dat) %in% c("vote"))]

df = data.frame(x, y)

# fit = stan_glm(y ~ -1 + ., data=df) --> This is if you want a no-intercept model!!
fit = stan_glm(y ~ ., data=df)

# We can access the posterior simulations by extracting the fitted model as a matrix in R:
sims = as.matrix(fit)

# To check that we understand how the summaries are defined, we can compute them directly
# from the simulations:

median = apply(sims, 2, median)
mad_sd = apply(sims, 2, mad)
print(cbind(median, mad_sd))

# mad_sd is the median absolute deviation, which is more stable than standard error,
# or also known as posterior standard deviation.

# For convenience, mad_sd is referred to as the standard error in practice.

# Something I have thought long and hard about is how to get the approximation of conditional
# distributions P(A | B) from the posterior simulations. I think I know how, now.

# The first step is to condition on the parameter B that you explicitly want to control.
# The second implicit step is that all other conditional parameters will be averaged over, eliminating
# them from the conditional distribution entirely. Something like this:

hist(sims[sims[,"year"] < 0.03, "growth"], main = "P(beta_Growth | beta_Year < 0.03)")

# Above is the conditional distribution P(beta_Growth | beta_Year < 0.03). Obviously one can
# have more conditions too.


# Predictions

# Point prediction using predict

df = data.frame(x, y)

# fit = stan_glm(y ~ -1 + ., data=df) --> This is if you want a no-intercept model!!
fit = stan_glm(y ~ ., data=df)

new = data.frame(year=2020, growth=2.0)

y_point_pred = as.numeric(predict(fit, newdata=new))

# We can also calculate this by hand:
a_hat = coef(fit)[1]
b_hat = coef(fit)[2:length(coef(fit))]
y_point_pred = as.numeric(a_hat + b_hat %*% t(new)) # This is how to do it in general, when you
                                                    # have multiple predictors.



# We can use posterior_linpred to get uncertainty in the value of the fitted regression line.
y_linpred = posterior_linpred(fit, newdata=new)

# By hand:
sims = as.matrix(fit)
a = sims[,1]
b = sims[,2:length(coef(fit))]
y_linpred = a + b %*% t(new)

hist(y_linpred, breaks = seq(min(y_linpred), max(y_linpred), length.out = 35))



# Finally, we can construct a vector representing predictive uncertainty in a single election:
y_pred = posterior_predict(fit, newdata=new)

# By hand:
n_sims = nrow(sims)
sigma = sims[,ncol(sims)]
y_pred = as.numeric(a + b %*% t(new)) + rnorm(n_sims, 0, sigma)

boom = hist(y_pred, breaks = seq(min(y_pred), max(y_pred), length.out=80))

# The mode of the histogram, by the way, is found like this:
boom$mids[which.max(boom$counts)]

y_pred_median = median(y_pred)
y_pred_mad    = mad(   y_pred)
win_prob      = mean(y_pred > 50) # Damn, Clinton had a 83.5% chance of winning the 2016 election?
                                  # What a joke.



# We can also use the 3 functions we discussed to generate a range of predicted values.
new_grid = data.frame(year=seq(2016, 2023, length.out=7), growth=seq(-2.0, 4.0, length.out=7))


############################################################
############################################################
y_point_pred_grid = predict(fit, newdata=new_grid)
y_linpred_grid    = posterior_linpred(fit, newdata=new_grid)
y_pred_grid       = posterior_predict(fit, newdata=new_grid)
############################################################
############################################################


cbind(y_point_pred_grid, y_linpred_grid, y_pred_grid)

rbind(y_point_pred_grid, "y_linpred_grid" = colMeans(y_linpred_grid), "y_pred_grid" = colMeans(y_pred_grid))

# Propagating uncertainty

# Let's say that, in advance of the election, our best estimate of economic growth
# was 2.0% but with some uncertainty that shall express as a normal distribution with
# standard deviation 0.3%. We can then propagate the uncertainty in this predictor to obtain
# a forecast distribution that more completely expresses our uncertainty.

n_sims = 1000
x_new = data.frame(year=rep(2019, n_sims), growth=rnorm(n_sims, 2.0, 0.3))
y_pred = rnorm(n_sims, a + (b %*% t(x_new)), sigma)

hist(y_pred, main = paste("mean: ", mean(y_pred), ", sd: ", sd(y_pred)),
             breaks = seq(min(y_pred), max(y_pred), length.out=35))

# Remember: We can compute the point prediction by
# predict()

# We can compute simulations of the linear predictor:
# posterior_linpred()

# We can also compute posterior predictive simulations:
# posterior_predict()
# These are posterior predictive checks.
y_rep = posterior_predict(fit)

par(mfrow=c(3, 3))
for (s in sample(n_sims, 9)){
  hist(y_rep[s,])
}


# R^2 scores
bayes_R2(fit) # Gives R^2 values for data!
# Easily can do this with loo estimates too.


# An easy posterior predictive check can be done using bayesplot and ppc_dens_overlay.
library("bayesplot")

sample_sims = sample(nrow(y_rep), 100)

ppc_dens_overlay(df$y, y_rep[sample_sims,])

posterior_predict(fit)

# Note how the observed data is about in the middle of the replicated data.
# Some simulations are more extreme, others less so. This means that the model
# is performing quite well, although it seems rather noisy!















