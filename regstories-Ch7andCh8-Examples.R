setwd("C:/Users/rcasu/OneDrive - The Ohio State University/OSU/demos/bayesian-data-analysis")

library("rstanarm")

dat = read.csv("data/ros_ch7_electionseconomy.txt", header = TRUE, sep = " ")

a = 46.3
b = 3.0
sigma = 3.9

x = dat$growth
n = length(x)

y = a + b*x + rnorm(n, 0, sigma)

fake = data.frame(x, y)

fit = stan_glm(y ~ x, data=fake)
print(fit)

# Interpretation: a, b, and sigma are taken to be the ground-truth of the system.
# We are checking the model coefficients against these ground-truth values.

# Pay attention to the standard errors (the MAD_SDs) of the fitted coefficients. Each one individually
# tells us what the uncertainty of that coefficient is.

# The sigma estimate, or residual standard error, tells us how good the regression model is in
# actually predicting the observations in the dataset.

# x here is about 3.3 +/- 0.5, which tells us that the true value is probably around 2 or 4 if the model
# assumptions are true.

# Additionally, the sigma being around 2.5 to 3 tells us that most of our observations are spread out around the
# line by around 3 response units. Depending on the application, this might be a good or bad sign of
# predictability of the model.



# Let's do a more formal comparison of coefficients.

b_hat = coef(fit)["x"]
b_se = se(fit)["x"]

cover_68 = abs(b - b_hat) < b_se
cover_95 = abs(b - b_hat) < 2*b_se

c(cover_68, cover_95)

# so, the true value of the coefficient lies within 1 standard error of the estimated coefficient.

# The confidence intervals worked once, but do they have the correct coverage probabilities--
# that is to say, do the intervals contain the true value the advertised percentage of the time?

# To check, we use the normal distribution.
# We can simulate the sampling distribution and thus compute the coverage of the confidence interval
# by embedding the data simulation, model fitting, and coverage checking in a loop and running 1000 times.

# This and other loops in this chapter could also be performed implicitly using the replicate function.

# Let's do it the replicate way.

fake_fit = function(x, n = length(x), a = 46.3, b = 3.0, sigma = 3.9){
  if (!(class(x) == "numeric")){stop("Error: x is not a numeric vector.")}
  
  y     = a + b*x + rnorm(n, 0, sigma)
  fake  = data.frame(x, y)
  fit   = stan_glm(y ~ x, data=fake, refresh=0) # refresh=0 suppresses output to console
  b_hat = coef(fit)["x"]
  b_se  = se(fit)["x"]
  cover_68 = abs(b - b_hat) < b_se
  cover_95 = abs(b - b_hat) < 2*b_se
  return(list("cover_68?" = cover_68, "cover_95?" = cover_95))
}

dat = read.csv("data/ros_ch7_electionseconomy.txt", header = TRUE, sep = " ")

x = dat$growth

fake_output = replicate(10, fake_fit(x))

c("68% Interval coverage percentage" = mean(apply(fake_output, 1, unlist)[,1]),
  "95% Interval coverage percentage" = mean(apply(fake_output, 1, unlist)[,2]))

# In this case, some of the coefficients that we constructed on the data are within
# 1 or 2 standard errors of the true coefficient, so not bad. But we could do better.

# The reason for this is that we have a sample size of 16, and thus our inferences should
# use the t-distribution.



fake_fit_t = function(x, n = length(x), a = 46.3, b = 3.0, sigma = 3.9){
  if (!(class(x) == "numeric")){stop("Error: x is not a numeric vector.")}
  
  y     = a + b*x + rnorm(n, 0, sigma)
  
  fake  = data.frame(x, y)

  fit   = stan_glm(y ~ x, data=fake, refresh=0) # refresh=0 suppresses output to console
  b_hat = coef(fit)["x"]
  b_se  = se(fit)["x"]
  
  t_68 = qt(0.5+0.68/2, n-2, lower.tail = TRUE)
  t_95 = qt(0.5+0.95/2, n-2, lower.tail = TRUE)
  
  cover_68 = abs(b - b_hat) < t_68 * b_se
  cover_95 = abs(b - b_hat) < t_95 * 2*b_se
  return(list("cover_68?" = cover_68, "cover_95?" = cover_95))
}

dat = read.csv("data/ros_ch7_electionseconomy.txt", header = TRUE, sep = " ")

x = dat$growth

fake_output = replicate(10, fake_fit_t(x))

c("68% Interval coverage percentage" = mean(apply(fake_output, 1, unlist)[,1]),
  "95% Interval coverage percentage" = mean(apply(fake_output, 1, unlist)[,2]))

# Much better, these look more accurate.



# Formulating comparisons as regression models
require("rstanarm")
rm(list = ls())

n_0 = 20
y_0 = rnorm(n_0, 2.0, 5.0)
fake_0 = data.frame(y_0)
print(y_0)

# We can estimate the mean of the population as:
mean(y_0)

# We can estimate the standard error of the mean as:
sd(y_0)/sqrt(n_0)

# We get the identical result using least squares regression on a constant term:
fit_0 = stan_glm(y_0 ~ 1, data=fake_0,
                 prior_intercept=NULL, prior=NULL, prior_aux=NULL)
print(fit_0)


# Takeaway: the average can be seen as the same as the intercept-only regression.


# The estimates of 3.3 +/- 1.1 for the intercept coefficient and 5 ish for the standard error
# are definitely noisy, but are still consistent with the data, and make sense given the low
# sample size that we have.


# Next, add in a new group: 30 observations from a population with
# mean 8.0 and standard deviation 5.0.

n_1 = 30
y_1 = rnorm(n_1, 8.0, 5.0)

# We can directly compare the averages in each group and compute the corresponding standard
# error.

diff = mean(y_1) - mean(y_0)
se_0 = sd(y_0)/sqrt(n_0)
se_1 = sd(y_1)/sqrt(n_1)
se = sqrt(se_0^2 + se_1^2)

# Alternatively, we can frame the problem as aregression by combining the data into a single
# vector, y, and then creating an indicator variable x.

n = n_0 + n_1
y = c(y_0, y_1)
x = c(rep(0, n_0), rep(1, n_1))
fake = data.frame(x, y)
fit = stan_glm(y ~ x, data=fake, prior_intercept=NULL, prior=NULL, prior_aux=NULL)
print(fit)

# The estimate of the slope, 4.1, is identical to the above difference in means.
# The standard error is nearly identical but differs slightly because the regression model
# estimates a single residual standard deviation parameter, as compared to the difference
# calculation which uses separate values of se_0 and se_1.

rm(list=ls())

x = 1:10
y = c(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
fake = data.frame(x, y)
fit = stan_glm(y ~ x, data=fake)
print(fit)
sims = as.matrix(fit)

# Easy 95% interval for the coefficient for x:
quantile(sims[, "x"], c(0.025, 0.975))

# The approximation to this is median +/- 2*mad_sd










# Problem 8.9: Leave one out cross-validation

# The first implementation is not LOOCV. I don't systematically go through
# all folds. In fact it might be a more robust indication of test error,
# as it's random sample of n-1 folds, but it's not LOOCV. Let's do that next.
rm(list=ls())
library("rstanarm")

setwd("C:/Users/rcasu/OneDrive - The Ohio State University/OSU/demos/bayesian-data-analysis")
dat = read.csv("data/ros_ch7_electionseconomy.txt", header = TRUE, sep = " ")

x = dat[, !(names(dat) %in% c("vote", "inc_party_candidate", "other_candidate"))]
y = dat[,  (names(dat) %in% c("vote"))]

df = data.frame(x, y)


# Report the estimated coefficients a and all the bs.

n_sims = 100

coefs = list()
res = list()

sigma_cv   = rep(NA, n_sims)
sigma_stan = rep(NA, n_sims)
sigma_freq = rep(NA, n_sims)

for (i in 1:n_sims){
  print(i)
  train_index = sample(1:nrow(x), nrow(x)-1, replace=FALSE)
  # test_index is just the complement of train_index.
  train = data.frame(x[train_index,],  "y" = y[ train_index]) # use - here, not !. ! is only for logicals. - is for numerics.
  test  = data.frame(x[-train_index,], "y" = y[-train_index])
  fit = stan_glm(y ~ ., data=train, refresh=0)
  coefs[[i]] = coef(fit)
  
  # Now, compute the residual to the held-out point.
  res[[i]] = test[,dim(test)[2]] - coefs[[1]][2:length(coefs[[i]])] %*% c(unlist(test[, -dim(test)[2]]))
  
  sigma_cv[i]   = sqrt(1/dim(train)[1] * sum(res[[i]])^2)
  sigma_stan[i] = median(as.matrix(fit)[, "sigma"])
  sigma_freq[i] = sqrt(1/(dim(train)[1]-2) * sum(res[[i]])^2)
}

par(mfrow = c(2, 2)) # 2-by-2 grid of plots
par(oma = c(4, 4, 0, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
par(mar = c(2, 2, 1, 1)) # make the plots be closer together

# # now plot the graphs with the appropriate axes removed (via xaxt and yaxt),
# # remove axis labels (so that they are not redundant with overall labels,
# # and set some other nice choices for graphics parameters
# plot(runif(10), xlab = '', ylab = '', xaxt = 'n', las = 1, ylim = c(0, 1))
# plot(runif(10), xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', ylim = c(0, 1))
# plot(runif(10), xlab = '', ylab = '', las = 1, ylim = c(0, 1))
# plot(runif(10), xlab = '', ylab = '', yaxt = 'n', ylim = c(0, 1))

hist(sigma_cv)
hist(sigma_stan)
hist(sigma_freq)

# print the overall labels
mtext('x-axis title', side = 1, outer = TRUE, line = 2)
mtext('y-axis title', side = 2, outer = TRUE, line = 2)

c("mean_cv" = mean(sigma_cv), "mean_stan" = mean(sigma_stan), "mean_freq" = mean(sigma_freq))



# LOOCV implementation
rm(list=ls())
library("rstanarm")

setwd("C:/Users/rcasu/OneDrive - The Ohio State University/OSU/demos/bayesian-data-analysis")
dat = read.csv("data/ros_ch7_electionseconomy.txt", header = TRUE, sep = " ")

x = dat[, !(names(dat) %in% c("vote", "inc_party_candidate", "other_candidate"))]
y = dat[,  (names(dat) %in% c("vote"))]

df = data.frame(x, y)


# Report the estimated coefficients a and all the bs.

n = length(y)

coefs = list()
res = list()

sigma_cv   = rep(NA, n)
sigma_stan = rep(NA, n)
sigma_freq = rep(NA, n)

for (i in 1:n){
  print(i)
  
  # test_index is just the complement of train_index.
  train = data.frame(x[-i,],  "y" = y[-i]) # use - here, not !. ! is only for logicals. - is for numerics.
  test  = data.frame(x[ i,],   "y" = y[ i])
  fit = stan_glm(y ~ ., data=train, refresh=0)
  coefs[[i]] = coef(fit)
  
  # Now, compute the residual to the held-out point.
  res[[i]] = test[,dim(test)[2]] - coefs[[1]][2:length(coefs[[i]])] %*% c(unlist(test[, -dim(test)[2]]))
  
  sigma_cv[i]   = sqrt(1/dim(train)[1] * sum(res[[i]])^2)
  sigma_stan[i] = median(as.matrix(fit)[, "sigma"])
  sigma_freq[i] = sqrt(1/(length(y[-i])-length(coef(fit))) * sum(residuals(fit)^2))
}

par(mfrow = c(2, 2)) # 2-by-2 grid of plots
par(oma = c(4, 4, 0, 0)) # make room (i.e. the 4's) for the overall x and y axis titles
par(mar = c(2, 2, 1, 1)) # make the plots be closer together

# # now plot the graphs with the appropriate axes removed (via xaxt and yaxt),
# # remove axis labels (so that they are not redundant with overall labels,
# # and set some other nice choices for graphics parameters
# plot(runif(10), xlab = '', ylab = '', xaxt = 'n', las = 1, ylim = c(0, 1))
# plot(runif(10), xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', ylim = c(0, 1))
# plot(runif(10), xlab = '', ylab = '', las = 1, ylim = c(0, 1))
# plot(runif(10), xlab = '', ylab = '', yaxt = 'n', ylim = c(0, 1))

hist(sigma_cv)
hist(sigma_stan)
hist(sigma_freq)

c("mean_cv" = mean(sigma_cv), "mean_stan" = mean(sigma_stan), "mean_freq" = mean(sigma_freq))

# So, I thought about this as I was sleeping last night.
# It makes sense that the Stan estimate of sigma is much lower than the other points.

# The reason is that the errors for the cv estimates are calculated with the test point in mind,
# whereas the error for the Stan model is calculated with the training data in mind.
# Thus, the Stan model is potentially underestimating the possible test error.

# Also worthy of noting is that the variance of the CV test error is higher because the
# errors are based on individual points, which have lower SNR ratios individually than
# collection of points.

# I meesed up the typical frequentist estimate because I forgot to put the square inside the
# sum() argument, so instead of calculating a sum of squares of the residuals, I was calculating
# the squared sum of residuals, which is a totally different thing.
# The RSS from the lm() fit and the one I calculated have much more parity now.


# 8.10 is an interesting example. What would you have to do to maximize the difference between
# sigma_cv and sigma_freq? The answer is to create highly variable data, perhaps even using
# heavy-tailed distributions to generate the data and approximate with a normal linear model.
# This way, you would have a few points that are very far from the center of mass of points, which
# would lead to large disparities between sigma_cv and sigma_freq.

# In essence, CV estimates of sigma are data-centric and model estimates of sigma are MODEL-centric.

# Also, I was curious about how to implement folds in data.
# Here's a potential approach.

# Note that the indices generated will be the test point, the rest of the data
# will be the training data.

n_folds = 2:(dim(dat)[1]-1)
n_folds = 15

for (n_fold in n_folds){
  indices = c(floor(seq(1, dim(dat)[1], length.out=n_fold+1)), dim(dat)[1]+10000)
  print(paste(n_fold, "folds."))
  
  for (i in 1:length(indices)){
    if (indices[i+1] <= dim(dat)[1] & i == 1){
      print(paste(indices[i], indices[i+1], sep= " "))}
    
    else if (indices[i+1] <= dim(dat)[1] & i > 1){
      print(paste(indices[i]+1, indices[i+1], sep= " "))}
    
    else {break}
  }}
}

















