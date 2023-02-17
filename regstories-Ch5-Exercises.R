# 5.1: Discrete Probability Simulation:

# Basketball player has 60% chance of making a shot.
# Keeps taking shots until he misses twice in a row.
# Assume shots are independent.

# Write an R function to simulate this process.

basketball_trial = function(n_shots){
  n_miss = 0
  n_made = 0
  
  
  for (i in 1:n_shots){
    counter = i
    
    if (n_miss >= 2) {break}
    
    # Shot trial
    shot = rbinom(1, 1, 0.6)
    
    if (shot == 0) {
      n_miss = n_miss + 1
    }
    
    else {
      n_made = n_made + 1
    }
  }
  
  return(list("Number of shots made" = n_made, "Total shots taken" = counter))
}

output = replicate(1000, basketball_trial(n_shots=500))

shots_made = unlist(output[1,])
shots_taken = unlist(output[2,])
prop_shots_made = shots_made / shots_taken

hist(shots_made, breaks=seq(floor(min(shots_made)), max(shots_made), 1))

c("average number of shots made" = mean(shots_made), "standard deviation of shots made" = sd(shots_made))

# Now, using your simulations, make a scatterplot of the number of shots the player will take
# and the proportion of shots that are successes.

plot(shots_taken, prop_shots_made)









# 5.2 Continuous Probability Simulation

# The logarithms of weights (in pounds) of men in the United States are approximately
# normally distributed with mean 5.13 and standard deviation 0.17
# Women's log weights are approximately normally distributed with
# mean 4.96 and standard deviation 0.20.

# Suppose that 10 adults selected at random step on an elevator with a capacity of 1750 pounds
# What is the probability that their total weight exceeds this limit?


elevator = function(){
  # First, select 10 adults.
  man_or_woman = sample(c("man", "woman"), size=10, replace=TRUE, prob=c(0.5, 0.5))
  
  # Let's do it that vectorized way, instead of using for loops.
  log_weights = ifelse(man_or_woman=="man", rnorm(10, mean=5.13, sd=0.17), rnorm(10, mean=4.96, sd=0.20))
  
  return(sum(exp(log_weights)))
}

total_weights = replicate(1000, elevator())

hist(total_weights)

# The estimated probability that total weight exceeds the elevator's limit:
sum(total_weights > 1750)/length(total_weights)









# 5.3 Binomial Distribution
# A player takes 10 basketball shots, with a 40% probability of making each shot.
# Assume the outcomes of the shots are independent.

# Write a line of R code to compute the probability that the player makes exactly 3 of the 10 shots.

dbinom(3, 10, 0.4)

# Incidentally, doing this for continuous distributions is a lot more complex and requires a little bit
# more care. The CDF of continuous distributions can be used to estimate
# probabilities too. It's not perfect, but nothing ever is.

# Write an R function to simulate the 10 shots. Loop this function 10,000 times
# and check that your simulated probability of making exactly 3 shots is close
# to the exact probability computed in a.

basketball_trial_2 = replicate(10000, sum(rbinom(10, 1, 0.4) == 1))

sum(basketball_trial_2 == 3)/length(basketball_trial_2) - dbinom(3, 10, 0.4) # small difference!










# 5.4 Demonstration of the Central Limit Theorem

# Let x = x_1 + ... + x_20, the sum of 20 independent uniform(0,1) random variables

# In R, create 1000 simulations of x and plot their histogram
# What is the normal approximation to this distribution provided by the Central Limit Theorem
# Overlay a graph of the normal density on top of the histogram
# Comment on any differences between the histogram and the curve

clt_trial = replicate(1000, sum(runif(20)))
clt_hist_info = hist(clt_trial)

x_norm = seq(min(clt_trial), max(clt_trial), length.out = 100)
y_norm = max(clt_hist_info$counts)/max(dnorm(x_norm, mean=mean(clt_trial), sd=sd(clt_trial))) * dnorm(x_norm, mean=mean(clt_trial), sd=sd(clt_trial))

points(x_norm, y_norm)











# 5.5

normal_difference = replicate(1000, mean(rnorm(100, 69.1, 2.9)) - mean(rnorm(100, 63.7, 2.7)))

hist(normal_difference)
c("mean" = mean(normal_difference), "standard deviation" = sd(normal_difference))











# 5.6 Propagation of Uncertainty
# Cost savings of $5 per unit, standard error of $4.
# Size of market is 40,000, standard error of 10,000.

savings_vec = rnorm(1000, 5, 4)
savings_vec[savings_vec < 0] = 0

market_vec = rnorm(1000, 40000, 10000)
market_vec[market_vec < 0] = 0

overall_vec = savings_vec * market_vec

hist(overall_vec)
c("Mean Savings" = mean(overall_vec), "Median Savings" = median(overall_vec), "Standard Dev" = sd(overall_vec))

# Interesting. We see an overall right skew to the simulation data, which implies that savings
# will be less than the mean. The standard deviation savings is on the magnitude of the mean/median
# savings, which makes me a little skeptical of what savings we can find.

# A follow-up simulation would have me testing the sensitivity of the savings to the standard
# errors of the savings and the total market.










# 5.7

iterate_5_7 = function(){
  y = rnorm(1000, 6, 4)
  n = length(y)
  estimate = mean(y)
  se = sd(y)/sqrt(n)
  
  int_50 = estimate + qnorm(c(.50-.50/2, .50+.50/2)) * se
  int_95 = estimate + qnorm(c(.50-.95/2, .50+.95/2)) * se
  
  return(list("estimate" = estimate, "50% CI" = int_50, "95% CI" = int_95))
}

simulation_5_7 = replicate(100, iterate_5_7())

est = unlist(simulation_5_7[1,])
conf_50 = sapply(simulation_5_7[2,], function(elem){c(elem[2], elem[1])})
conf_95 = sapply(simulation_5_7[3,], function(elem){c(elem[2], elem[1])})

n_rep = length(simulation_5_7[1,])

par(mar=c(3,3,0,0), mgp=c(1.5,.5,0), tck=-.01)
plot(c(-2, n_rep+2), range(conf), bty="l", xlab="Simulation", ylab="Estimate, 50%, and 95% confidence interval", xaxs="i", yaxt="n", type="n")
axis(2, seq(-10,20,10))

points(1:n_rep, est, pch=20)
abline(6, 0, col="gray")
for (i in 1:n_rep){
  lines(c(i,i), conf_50[c(1,2), i], lwd=2)
  lines(c(i,i), conf_95[c(1,2), i], lwd=.3)
}










# 5.8

iterate_5_8 = function(){
  y = rnorm(100, 0.10, 0.17)
  n = length(y)
  estimate = mean(y)
  se = sd(y)/sqrt(n)
  
  int_95 = estimate + qnorm(c(.50-.95/2, .50+.95/2)) * se
  
  return(list(int_95))
}

simulate_5_8 = replicate(1000, iterate_5_8())

int_95_lo = sapply(simulate_5_8, function(elem){elem[1]})
int_95_hi = sapply(simulate_5_8, function(elem){elem[2]})

int_95_lo = unlist(int_95_lo)
int_95_hi = unlist(int_95_hi)

true_parameter = 0.10

# Proportion of intervals that include the true value.
sum(int_95_lo < true_parameter & int_95_hi < true_parameter)/length(int_95_lo)


# Compute the average and standard deviation of the 1000 point estimates;
# these represent the mean and standard deviation of the sampling distribution of the
# estimated treatment effect.

iterate_5_8 = function(){
  y = rnorm(100, 0.10, 0.17)
  n = length(y)
  estimate = mean(y)
  se = sd(y)/sqrt(n)
  
  int_95 = estimate + qnorm(c(.50-.95/2, .50+.95/2)) * se
  
  return(estimate)
}

simulate_5_8_c = replicate(1000, iterate_5_8())

c(mean(simulate_5_8_c), sd(simulate_5_8_c))










# 5.12 Randomization ***Important***

# Write a function in R to assign n items to treatment and control conditions
# under the following assignment procedures:

# Independent random assignment: Each item is independently randomly assigned to treatment
# or control with probabilities p and 1-p

ind_assign = function(vec, p=0.5){
  assignment = sample(c("trt", "ctrl"), size=length(vec), prob=c(p, (1-p)), replace = TRUE)
  return(rbind(assignment, vec))
}

ind_assign(1:20)


# Complete random assignment: The n items are randomly partitioned into np items that
# receive the treatment, and the other n*(1-p) are assigned to control

comp_assign = function(vec, p=0.5){
  trt = rep("trt", length(vec)*p)
  ctrl = rep("ctrl", length(vec)*(1-p))
  return(rbind(sample(c(trt, ctrl)), vec))
}

comp_assign(1:20)

# Matched Pairs
# This is the simplest version of block random assignment.
# The n items come sequentially in n/2 pairs. Within each pair, one item is randomly chosen
# for treatment and one for control. In other words, p = 0.5.

pair_assign = function(vec, p=0.5){
  assign = rep(NA, length(vec))
  for (i in seq(1, length(vec), 2)){
    flip = rbinom(1, 1, p)
    
    if (flip == 0){
      assign[i] = "trt"
      assign[i+1] = "ctrl"
    }
    
    else if (flip == 1) {
      assign[i] = "ctrl"
      assign[i+1] = "trt"
    }
    print(assign)
  }
  return(rbind(assign, vec))
}

pair_assign(1:20)
  