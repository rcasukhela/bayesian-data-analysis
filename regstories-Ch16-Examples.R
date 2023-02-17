rm(list=ls())
setwd("C:/Users/rcasu/OneDrive - The Ohio State University/OSU/demos/bayesian-data-analysis/")

library("rstanarm")
library("bayesplot")

# The most general and often the clearest method for studying the statistical properties of a proposed design is to simulate the data that might be collected along with the analyses that could be performed. We demonstrate with an artificial example of a randomized experiment on 100 students designed to test an intervention for improving final exam scores.

n = 100
y_if_control = rnorm(n, 60, 20)
y_if_treated = y_if_control + 5

# In this very simple model, the intervention would add 5 points to each student's score.

# We then assign treatments (z=0 for control or 1 for treatment), which then determine which outcome is observed for each person:

z = sample(rep(c(0, 1), n/2))
y = ifelse(z==1, y_if_treated, y_if_control) # condition to check is if z==1, if yes then pick out the y_treat value, if no then pick out the y_control value.

fake = data.frame(y, z)

# Having simulated the data, we can now compare treated to control outcomes and compute the standard error for the difference.

diff = mean(y[z==1]) - mean(y[z==0])
se_diff = sqrt(sd(y[z==0])^2/sum(z==0) + sd(y[z==1])^2/sum(z==1)) # sum(z==0) and sum(z==1) give you the number of samples that are in both treatment and control.



# Equivalently, we can run the regression:

fit_1a = stan_glm(y ~ z, data=fake)
fit_1a

# The parameter of interest here is the coefficient of z, and its standard error is 3.8, suggesting that, under these conditions, a sample size of 100 would not be enough to get a good estimate of a treatment effect of 5 points.

# The standard error of 4.3 is fairly precisely estimated, as we can tell because the uncertainty in sigma is low compared to its estimate.

# When looking at the above simulation result to assess this design choice, we should focus on the standard error of the parameter of interest (in this case, z).


# You can also include a pre-test.

n = 100

true_ability = rnorm(n, 50, 16) # Each student is given a raw ability, unobserved.
x = true_ability + rnorm(n, 0, 12) # The students take a pre_test, which is supposed to measure this.
y_if_control = true_ability + rnorm(n, 0, 12) + 10 # Students who get the control do decently on the post-test.
y_if_treated = true_ability + rnorm(n, 0, 12) + 15 # Students who get the treatment should do better on the post-test, based on our hypothesis.

# As above, assign treatments, construct the observed outcome, and put the data into a dataframe.
z = sample(rep(c(0, 1), n/2))
y = ifelse(z==1, y_if_treated, y_if_control)
fake_2 = data.frame(x, y, z)

# The simple comparison is equivalent to a regression on the tratment indicator (this is mentioned in previous parts of the book).

fit_2a = stan_glm(y ~ z, data=fake_2) # which doesn't take into account the pre-test.

fit_2b = stan_glm(y ~ z + x, data-fake_2) # In this case, with the strong dependence between pre and post test, this adjustment has reduced the residual standard deviation by about a third.


# Suppose we are concerned about bias in teh treatment assignment. We can simulate that too.

# For example, suppose that school administrators, out of kindness, are more likely to give the treatment to students who are performing poorly. We could siulate this behavior with an unequal probability assignment rule such as P(z_i = 1)= invlogit(-(x_i - 50)/20), where we have chosen the logistic curve for convenience and set its parameters so that the probability averages to approximately 0.5, with a bit of variation from one end of the data to the other.

# In this case, a manual simple comparison will yield a biased estimate, while the linear regression adjusting for pre test is better.

# To see this, we should not just perform one simulation. As discussed earlier, not much can be learned from the estimated obtained from any single simulation. Instead we first write a function to simulate the fake data, assign the treatments, and perform the simple comparison and the regression adjusting for pre-test:

z = rbinom(n, 1, invlogit(-(x-50)/20)) # This does a Bernoulli trial with the probability based on the pre-test of the student. In this case, students who perform poorly might be preferred for treatment.


experiment = function(n){
  true_ability = rnorm(n, 50, 16)
  x = true_ability + rnorm(n, 0, 12)
  y_if_control = true_ability + rnorm(n, 0, 12) + 10
  y_if_treated = true_ability + rnorm(n, 0, 12) + 5

  z = rbinom(n, 1, invlogit(-(x-50)/20))
  y = ifelse(z==1, y_if_treated, y_if_control)
  
  fake_3 = data.frame(x, y, z)
  fit_3a = stan_glm(y ~ z, data=fake_3, refresh=0)
  fit_3b = stan_glm(y ~ z + x, data=fake_3, refresh=0)
  
  result = rbind(c(coef(fit_3a)["z"], se(fit_3a)["z"]), c(coef(fit_3b)["z"], se(fit_3b)["z"]))
}

n = 100
n_loop = 3

# So, this is cool. Looks like an array can store tables per iteration. Let's break this down: 
# First, we obviously instantiate the array. The array needs 3 arguments to work for our case.
# Data: Instantiate with an NA.
# Dimensions: Supply an atomic vector containing the dimensions. For our case, we want a 3 dimensional array. Along the "Z" axis of this array, we store the iterations of the simulations that we run.
# Dimnames: Each of the length of the dimnames you supply for each dimension ought to line up with the number of elements per dimension you give!
results = array(NA, c(n_loop, 2, 2),
                dimnames=list(1:n_loop, c("simple", "adjusted"), c("estimate", "se")))

# Below, you can see how to index and populate to the array. Nice!
for (loop in 1:n_loop){
  results[loop,,] = experiment(n)
}

results_avg = apply(results, c(2, 3), mean)

#########################################################################################################
#########################################################################################################

# Chapter 16 is by far the most confusing chapter in this entire book, which is frustrating considering
# how necessary power analysis is for my work.

# I need to develop a personalized workflow for determining power/sample size for whatever studies I
# want to run.

# I seem to resonate most with the idea of running my own simulations, and fitting a regression to the data to estimate power.
# It seems to be the most clear-cut and straightforward way of doing things.

# The first experiment I want to run is with two groups, control and treatment. I want to change levels
# of within-group variation between the two groups, and see how it affects my estimates.

n_total = 3000

ctrl_sample_prop = 0.5
ctrl_mean = 5
ctrl_sd   = 0.1

trt_sample_prop  = 1-ctrl_sample_prop
trt_mean  = 10
trt_sd    = 0.1

# Generate responses. Here we just stratify by control and treatment, but you could do more fancy things as well.
ctrl = rnorm(n_total*ctrl_sample_prop, ctrl_mean, ctrl_sd)
trt  = rnorm(n_total*trt_sample_prop , trt_mean , trt_sd )

# z just creates the treatment assignments with the proper proportions per n_total.
z = c(rep(0, n_total*ctrl_sample_prop), rep(1, n_total*trt_sample_prop))
# Shuffle z.
z = sample(z)
# Assign treatments as per the order of z, and measure response y.
y = ifelse(z==0, ctrl, trt) # condition to check is if z==1, if yes then pick out the y_treat value, if no then pick out the y_control value.

fake = data.frame(y, z)

fit = stan_glm(y ~ z, data=fake)
sims = as.matrix(fit)

plot(fake$z, fake$y, xlab = "Groups", ylab="Response")

n_lines = nrow(sims)

for (j in sample(n_lines, 20)){
  curve(sims[j,1] + sims[j,2]*x, col="gray", lwd=0.5, add=TRUE)
}

curve(coef(fit)[1] + coef(fit)[2]*x, add=TRUE)



# Playing around with this block of code reveals a few things to me. Firstly, the residual standard deviation is an estimate of the average within-group variance. Secondly, the standard error of our coefficients of main effects and interactions can tell us how closely we can estimate those parameters.

# 12/7/22: From a Bayesian perspective, I wouldn't mess around too much with power. Of course, this is because power is an inherently frequentist concept. Instead, I'd look at posterior summaries of parameters of interest and datasets from the posterior predictive.

# Understanding how the posterior summaries can tell us about 'power' easily enough.

hist(sims[, select=c('z')]) # This gives us a general idea about the spread of possible z values, but what if we want a numerical estimate?

# Say we want to detect an effect size of around 5.
sims = data.frame(fit)

sum(sims$z <= 5.1 & sims$z >= 4.9)/length(sims$z) # You can mess around with posterior summaries of the parameter of interest.

n_sims = dim(sims)[1]

y_rep = posterior_predict(fit)

subset = sample(n_sims, 100)
ppc_dens_overlay(fake$y, y_rep[subset,])

fit

# I think PPD checks are really only for model fit.

# I'll have to think a bit harder about the posterior predictive. Perhaps the thing to do is to look at what the model predicts for given inputs?

new  = data.frame(z = rbinom(100, 1, 0.5))

epred = posterior_epred(fit, newdata=new) # Each row is the predicted dataset!!!!

# Be careful: posterior_epred is a generalized version of posterior_linpred. To properly propagate uncertainty, use posterior_predict.
epred = posterior_predict(fit, newdata=new) # Each row is the predicted dataset!!!!

test = cbind(new, t(epred[sample(20), ]))

hist(as.numeric(test[test$z==1,-1][1,]))

hist(as.numeric(test[test$z==0,-1][1,]))

# So, this comparison makes a LOT more sense. This tells us what the model believes the response to be
# based on our inputs, which can be a random selection of treatments. This is probably more useful than a posterior predictive check for assessing the study design.

# The lesson here is that PPD checks are good for checking the validity of the model, while predictions on new inputs from the data can be used to compare to the original data.

# As always, posterior summary of parameters will give you an idea of how accurately one may estimate the effect size. I wouldn't try anything fancy with proportions and percentages here, as the distribution can change so much. I'd instead just rely on mean and sd of the distribution, as well just looking at the distribution.

















