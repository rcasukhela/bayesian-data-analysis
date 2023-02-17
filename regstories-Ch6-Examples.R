library("rstanarm")

# standard deviation of the first example is 0.5.
# The book says that we should expect roughly 2/3 of the points to fall within
# +/- 1 standard error of the line.

# To empirically see this, let's simulate:
sigma = 0.5
num_samples = 100

norm_sim = function(num_samples, sigma){
  norm_sim = rnorm(num_samples, 0, sigma)
  return(sum(norm_sim > -sigma & norm_sim < sigma)/num_samples)
  
}


mean(replicate(10000, norm_sim(num_samples, sigma)))
# so, 1 standard error is the magnitude of 1*sigma.

x = 1:20
n = length(x)
a = 0.2
b = 0.3
sigma = 0.5

y = a + b*x + sigma*rnorm(n)

fake = data.frame(x, y)



fit_1 = stan_glm(y ~ x, data=fake)
print(fit_1, digits=3)

plot(fake$x, fake$y, main="Data and fitted regression line")
a_hat = coef(fit_1)[1]
b_hat = coef(fit_1)[2]
abline(a_hat, b_hat, lty=2)
