rm(list=ls())
setwd("C:/Users/rcasu/OneDrive - The Ohio State University/OSU/demos/bayesian-data-analysis/")

library("rstanarm")
library("bayesplot")

electric = read.csv('data/ros_ch20_electric.csv', header=TRUE)

# We know from previous chapters, that when the probability of assignment to the treatment varies
# with the level of a pre-treatment variable, it is important to account for that variable when estimating
# the effect of the treatment.

# A simple way to do this with a continuous covariate is by including it as a regression predictor.

# For our example, we add to our data frame a variable called `supp` that that equals:
# 0 for the replacement form of the treatment,
# 1 for the supplement, and
# NA for the controls.
# The last level drops the control observations from the analysis.
# (This is already included in the dataframe, so don't worry).

electric$grade = factor(electric$grade)
electric$supp = factor(electric$supp)

fit_1 = stan_glm(post_test ~ supp + pre_test + grade + grade:supp, data=electric, subset = (!is.na(supp)))

#------------------------
# Median MAD_SD
# (Intercept) 61.8    3.3  
# supp        13.6    4.2  
# pre_test     0.5    0.1  
# grade       -1.4    1.9  
# supp:grade  -4.1    1.6  
#------------------------

# From this fit we can tentatively conclude that there is some positive association with supplementing versus replacing on the post-test, that there might possibly be diminishing returns on post-test scores as you go up in grade, and that the difference between supplementing and replacing as you go up in grade seems to increase. The last two associations are difficult to assess because the grade coefficient has such a high standard error compared to the magnitude of the coefficient.

electric_subset = subset(electric, subset=!is.na(electric$supp) & electric$supp == 0)

par(mfrow=c(2, 1), mar=c(2, 2, 2, 2))
plot(electric_subset$pre_test, electric_subset$post_test, pch=0, col=as.factor(electric_subset$grade))
legend(x = "bottomright",, box.lwd = 2 , title="Grades",
       legend=c("G1", "G2", "G3", "G4"),
       fill=c("black", "red", "green", "blue"))


electric_subset = subset(electric, subset=!is.na(electric$supp) & electric$supp == 1)
plot(electric_subset$pre_test, electric_subset$post_test, pch=1, col=as.factor(electric_subset$grade))
legend(x = "bottomright",, box.lwd = 2 , title="Grades",
       legend=c("G1", "G2", "G3", "G4"),
       fill=c("black", "red", "green", "blue"))

# It's clear that fitting separate regressions would be a good idea for multiple reasons. For one, the within groups standard deviation would be a lot smaller, though it is estimated precisely given the fit of the model. Secondly, there is a clear discrepancy between the pre&post test scores when grouped by grade.

# Regardless, this is one of those scenarios where I would trust the graph over the model more. The model says that the interaction seems to be distinguishable from the noise, but I really don't buy that. I think it's clear that supplementation vs replacement of the tv show has some effect, but the standard error is to high for understanding the effect of supplementation on grade. Frankly, it seems the effect of supp on increasing grade is not clear at all.




#############################################################################################
# Let's spend some time thinking about the observational study example presented here for childcare.
# I need to understand how to examine imbalance, lack of overlap, subclassify, and assign propensity scores for matching first.
#############################################################################################



rm(list=ls())
setwd("C:/Users/rcasu/OneDrive - The Ohio State University/OSU/demos/bayesian-data-analysis/")

library("rstanarm")
library("bayesplot")

dat = read.csv('data/ros_ch20_cc2.csv', header=TRUE)

# Some quick tips: Here's how to get all integer columns:
dat_int = cbind(dat[, sapply(dat, is.integer) == 1], dat["treat"])

# Here's how to get all numeric columns:
dat_num = cbind(dat[, sapply(dat, is.numeric) == 1], dat["treat"])

# Get all logical columns:
dat_logi = cbind(dat[, sapply(dat, is.logical) == 1], dat["treat"])

# Get all character columns:
dat_char = cbind(dat[, sapply(dat, is.character) == 1], dat["treat"])

# First thing's first, let's take a look at imbalance.
ctrl_int = dat_int[dat_int[, 1] & dat["treat"] == 0, ]
trt_int  = dat_int[dat_int[, 1] & dat["treat"] == 1, ]
# Here's how you can quickly examine proportions of categorical variables per treatment in your dataset, visually.
par(mfrow=c(1,2), cex=0.6)
for (i in 1:(length(colnames(dat_int))-sum(names(dat_int) %in% c("treat")))){
  barplot(
    table(
      ctrl_int$treat,
      t(ctrl_int[, !(names(ctrl_int) %in% c("treat"))][i])
    ),
    main=paste("Control", names(ctrl_int)[i], "mean", round(mean(unlist(ctrl_int[i])), 1), "sd", round(sd(unlist(ctrl_int[i])), 1))
  )
  
  barplot(
    table(
      trt_int$treat,
      t(trt_int[, !(names(trt_int) %in% c("treat"))][i])
    ),
    main=paste("Treatment", names(trt_int)[i], "mean", round(mean(unlist(trt_int[i])), 1), "sd", round(sd(unlist(trt_int[i])), 1))
  )
  
  
}
dev.off()
dev.off()

# You can also do this in a tabulated format.
J = c(length(colnames(dat_int))-sum(names(dat_int) %in% c("treat")), 6)
int_cols_summary = as.data.frame(array(NA, c(J[1], J[2])))
colnames(int_cols_summary) = c("Column Name*", "CTRL-TRT Std. Mean*", "CTRL-TRT Min*", "CTRL-TRT Median*", "CTRL-TRT Max*", "CTRL-TRT SD*")

for (i in 1:(length(colnames(dat_int))-sum(names(dat_int) %in% c("treat")))){
  int_cols_summary[i,] = c(names(dat_int)[i],
      abs(round(mean(unlist(ctrl_int[i])), 1) - round(mean(unlist(trt_int[i])), 1) / sqrt(sd(unlist(ctrl_int[i]))^2 + sd(unlist(trt_int[i]))^2)),
      round(min(unlist(ctrl_int[i])), 1) - round(min(unlist(trt_int[i])), 1),
      round(median(unlist(ctrl_int[i])), 1) - round(median(unlist(trt_int[i])), 1),
      round(max(unlist(ctrl_int[i])), 1) - round(max(unlist(trt_int[i])), 1),
      round(sd(unlist(ctrl_int[i])), 1) - round(sd(unlist(trt_int[i])), 1))
}
int_cols_summary

# The next step is to figure out imbalance on the continuous variables as well. This is a pretty tedious task and would require me to actually understand the data, which would be difficult here. But the idea is the same, maybe just pick the variables that are individually correlated with the response highly and just look at those.

# I can diagnose lack of overlap as well.






# Let's skip to trying to estimate ATE and ATT.
fit_1 = lm(ppvtr.36 ~ . + treat*momed, data=dat)
summary(fit_1, digits=2)

# Alright.. seems like there is more data wrangling to do before we can just estimate the treatment effect. Let's just assume that momed is the only covariate just to make things easy.

fit_2 = stan_glm(ppvtr.36 ~ factor(momed) + factor(treat) + factor(treat)*factor(momed), data=dat)

# Comparing the coefficients of treat across subclasses, we see that the regression returns the treatment effect estimate for each subclass with associated standard error pretty well, when compared to figure 20.12.

# Momed: not high school graduate = 9.3 +/- 1.5, regression = 9.3 +/- 1.7
# Momed: high school grad         = 4.1 +/- 1.9, regression = (9.3-5.3=)4.1 +/- 2.6
# Momed: some college             = 7.9 +/- 2.4, regression = (9.3-1.5=)7.8 +/- 3.3
# Momed: College graduate         = 4.6 +/- 2.3, regression = (9.3-4.7=)4.6 +/- 3.8

# Standard error is higher for the model but that might also just be because the Bayesian model is not very sure of what's up.

# We would need to run a separate regression on JUST treatment and momed, no interaction, to get the average treatment effect.

fit_3 = stan_glm(ppvtr.36 ~ factor(momed) + factor(treat), data=dat)
fit_3

# Median MAD_SD
# (Intercept)    77.4    0.5  
# factor(momed)2 10.3    0.7  
# factor(momed)3 11.9    0.8  
# factor(momed)4 24.5    1.1  
# factor(treat)1  7.0    1.2  --> ******That's your ATE.******

# Auxiliary parameter(s):
#   Median MAD_SD
# sigma 18.8    0.2

# Don't worry about the ATT for now. I know that I haven't finished the example completely, but I do at least have a few ways to look at imbalance and lack of overlap. I also have ways to subclassify based on some significant covariates and estimate effect sizes that way, as well as a quick method to determine the ATE, all via regression.

# It's a good idea to pause here and soak in everything that I learned today. The next step is to look at the propensity score matching example, it's likely that most of my questions will be answered when I look through that section.










#############################################################################################
# Section 20.7: Propensity Score Matching for the Child Care Example
#############################################################################################

rm(list=ls())
setwd("C:/Users/rcasu/OneDrive - The Ohio State University/OSU/demos/bayesian-data-analysis/")

library("rstanarm")
library("bayesplot")
library("MatchIt")
library("cobalt")
library("lattice")
library("ggplot2")

dat = read.csv('data/ros_ch20_cc2.csv', header=TRUE)

# An easy way to look at histograms by group.
#library('lattice')
histogram(~ income | momed, data = dat)

# Do propensity score matching:
p_scores = matchit(treat ~ momed + income, method="nearest", data=dat)
match = match.data(p_scores)

# And plot covariates by propensity score to example imbalance first, then lack of overlap.
# If this works out, then you've matched relatively well.
ggplot(match, aes(x=distance, y=momed, color=as.factor(treat))) + geom_point(alpha=0.2, size=1.3) + geom_smooth(method="loess", se=F) + theme_bw()

# You can examine the means of each treatment in the matched dataset this way.
# I'm sure I can figure out how to plot something like this.
aggregate(match[,!names(match) %in% c("treat")], by=list(match$treat), mean)

# Might be good to figure out how to calculate standardized mean easily.
bal.tab(p_scores, thresholds = c(m = .1), un = TRUE)

# Distributional balance plot
bal.plot(p_scores, var.name = "income")

# Distributional balance for the propensity scores
bal.plot(p_scores, var.name = "distance", mirror = TRUE, type = "histogram")

# Here are your absolute standardized differences in means and also variance ratios!!
love.plot(p_scores, stats = c("mean.diffs", "variance.ratios"),
          thresholds = c(m = .1, v = 2), abs = TRUE, 
          binary = "std",
          var.order = "unadjusted")

# Perfect. Now I have all tools to assess imbalance and overlap.
# Summary: MatchIt is awesome, so is cobalt.

# What's next? Well.. good matching is an iterative process. Once you have the propensity scores you are happy with, you can fit a regression on the matched, or restructured data, and attempt to make a causal inference about the quantity or group of interest. Done!!















