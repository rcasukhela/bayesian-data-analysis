rm(list=ls())
setwd("C:/Users/rcasu/OneDrive - The Ohio State University/OSU/demos/bayesian-data-analysis/")

library("rstanarm")
library("bayesplot")

J = c(2, 4, 4)
poststrat = as.data.frame(array(NA, c(prod(J), length(J)+1)))

colnames(poststrat) = c("sex", "age", "eth", "N")
count = 0

# This loop just creates all possible combinations of our factors.
for (i1 in 1:J[1]){
  for(i2 in 1:J[2]){
    for(i3 in 1:J[3]){
      count = count + 1
      poststrat[count, 1:3] = c(i1, i2, i3)
    }
  }
}

# Next, make up numbers for the populations of the cells and enter into the table.
p_sex = c(0.52, 0.48)
p_age = c(0.2, 0.25, 0.3, 0.25)
p_age_male = c(0.2, 0.25, 0.4, 0.15)
p_age_female = c(0.2, 0.25, 0.3, 0.25)
p_eth = c(0.7, 0.1, 0.1, 0.1)

# The book example assumes independence between sex, age, and ethnicity. Here I have added a dependence on number of people in an age bracket due to sex, as women tend to outlive men. This shows how we can add dependence based on individual cells. Nice.
for (j in 1:prod(J)){
  if (poststrat$sex[j] == 1){
    poststrat$N[j] = 250e6 * p_sex[poststrat[j, 1]] * p_age_male[poststrat[j, 2]] * p_eth[poststrat[j, 3]]
  }
  else{
    poststrat$N[j] = 250e6 * p_sex[poststrat[j, 1]] * p_age_female[poststrat[j, 2]] * p_eth[poststrat[j, 3]]
  }
}

# Then, hypothesize the nonresponse pattern:
p_response_baseline = 0.1
p_response_sex = c(1, 0.8)
p_response_age = c(1, 1.2, 1.6, 2.5)
p_response_eth = c(1, 0.8, 0.7, 0.6)
p_response = rep(NA, prod(J))

# I kept things simple, but look above as to how to make more complicated patterns based on the factors that we have.
for (j in 1:prod(J)){
  p_response[j] = p_response_baseline * p_response_sex[poststrat[j, 1]] * p_response_age[poststrat[j, 2]] * p_response_eth[poststrat[j, 3]]
}

# Sample from the assumed population with the assumed nonresponse probabilities.
n = 1000
people = sample(prod(J), n, replace=TRUE, prob=poststrat$N*p_response)

n_cell = rep(NA, prod(J))
for (j in 1:prod(J)){
  n_cell[j] = sum(people==j)
}

print(cbind(poststrat, n_cell/n, poststrat$N/sum(poststrat$N)))

# Assume the survey responses come from a logistic regression wiht these coefficients:
coef_intercept = 0.6
coef_sex = c(0, -0.2)
coef_age = c(0, -0.2, -0.3, -0.4)
coef_eth = c(0, 0.6, 0.3, 0.3)

# Again, you CAN generate more complex data with interactions. I'm choosing not to do it here, but just follow the example above. For interactions, you'll have to multiply the factors together.

# Anyway, the probabilities are as follows:
prob_yes = rep(NA, prod(J))
for (j in 1:prod(J)){
  prob_yes[j] = invlogit(coef_intercept + coef_sex[poststrat[j, 1]] + coef_age[poststrat[j, 2]] + coef_eth[poststrat[j, 3]])
}

# Generate the responses:
y = rbinom(n, 1, prob_yes[people])

# Fit a logistic regression to the data.
sex = poststrat[people, 1]
age = poststrat[people, 2]
eth = poststrat[people, 3]

fake = data.frame(y, sex, age, eth)
fit = stan_glm(y ~ factor(sex) + factor(age) + factor(eth), family=binomial(link="logit"), data=fake)

# Predict,
pred_sim = posterior_epred(fit, newdata=as.data.frame(poststrat))
pred_est = colMeans(pred_sim)
print(cbind(poststrat, prob_yes, pred_est))

# and poststratify.
poststrat_est = sum(poststrat$N*pred_est)/sum(poststrat$N)
round(poststrat_est, 2)

# This number gives us our point estimate of the overall probability of answering favorably to our yes/no question or whatever, across strata. To get inferential uncertainty, we can work with the matrix of posterior simulations.

poststrat_sim = pred_sim %*% poststrat$N / sum(poststrat$N)
round(c(mean(poststrat_sim), sd(poststrat_sim)), 3)

# The whole idea is to be able to take a sample and generalize your results of the model fit to the rest of the population.







#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################

# Missing Data

# Alright, there's a lot here to possibly cover. I'm just going to blindly rewrite the code that they have, in the off-chance that it becomes useful.

rm(list=ls())
setwd("C:/Users/rcasu/OneDrive - The Ohio State University/OSU/demos/bayesian-data-analysis/")

library("rstanarm")
library("bayesplot")

SIS = read.csv('data/ros_ch17_SIS.csv', header=TRUE)

topcode = function(a, top){
  ifelse(a>top, top, a)
}
SIS$earnings_top = topcode(SIS$earn, 100) # earnings are in thousands.

# First, fit the regression on the non-missing values. Note the use of the subset argument here.
fit_imp_1 = stan_glm(earnings ~ male + over65 + white + immig + educ_r + workmos + workhrs_top + any_ssi + any_welfare + any_charity, data=SIS, subset=earnings>0)

# The next step is to feed in the whole dataset with the exclusion of the response, back into the model.
SIS_predictors = SIS[, c("male", "over65", "white", "immig", "educ_r", "workmos", "workhrs_top", "any_ssi", "any_welfare", "any_charity")]

# This will give us predictions on the line, which are overly optimistic and do not reflect the uncertainty expressed in the model very well.
pred_1 = predict(fit_imp_1, newdata=SIS_predictors)
pred_1 = topcode(pred_1, 100)

# We could use posterior_predict for that purpose:
pred_2 = posterior_predict(fit_imp_1, newdata=SIS_predictors)
pred_2 = topcode(pred_2, 100)

impute = function(a, a_impute){
  ifelse(is.na(a), a_impute, a)
}
SIS$earnings_imp_1 = impute(SIS$earnings, pred_1)
SIS$earnings_imp_2 = impute(SIS$earnings, pred_2)

# Tough to tell any differences here, so just use posterior_predict. predict just gives you deterministic imputation.













