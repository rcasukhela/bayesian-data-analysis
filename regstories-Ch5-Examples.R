# Accounting for twins

# Description: There is a 1/125 chance that a birth event results in fraternal twins,
# of which each has an approximate 49.5% chance of being a girl, and a 1/300 chance of
# identical twins, which have an approximate 49.5% chance of being a pair of girls.

# We will simulate 400 birth events like this:

birth_type = sample(c("fraternal twin", "identical twin", "single birth"    ), size=400, replace=TRUE,
               prob=c( 1/125,            1/300,            1 - 1/125 - 1/300)
             )

girls = rep(NA, 400)

for (i in 1:400){
  if (birth_type[i] == "single birth") {girls[i] = rbinom(1, 1, 0.488)}
  else if (birth_type[i] == "identical twin") {girls[i] = 2*rbinom(1, 1, 0.495)}
  else if (birth_type[i] == "fraternal twin") {girls[i] = rbinom(1, 2, 0.495)}
}

n_girls = sum(girls)
n_girls


# Description: Now, do that 1000 times to get a distribution of data.

birth_type = sample(c("fraternal twin", "identical twin", "single birth"    ), size=400, replace=TRUE,
                    prob=c( 1/125,            1/300,            1 - 1/125 - 1/300)
)

n_sims = 1000
n_girls = rep(NA, n_sims)
girls = rep(NA, 400)
for (s in 1:n_sims){
for (i in 1:400){
  if (birth_type[i] == "single birth") {girls[i] = rbinom(1, 1, 0.488)}
  else if (birth_type[i] == "identical twin") {girls[i] = 2*rbinom(1, 1, 0.495)}
  else if (birth_type[i] == "fraternal twin") {girls[i] = rbinom(1, 2, 0.495)}
  n_girls[s] = sum(girls)
}}


sum(n_girls)


# We can do this without using for loops. The below method is faster, but confusing as shit.
# Personally, the speed of analysis wouldn't bother me unless I was doing serious optimization.
# Just stick with the above code.

birth_type = sample(c("fraternal twin", "identical twin", "single birth"    ), size=400, replace=TRUE,
                    prob=c( 1/125,            1/300,            1 - 1/125 - 1/300)
)

girls =
  ifelse(birth_type=="single birth", rbinom(400, 1, 0.488),
  ifelse(birth_type=="identical twin", 2*rbinom(400, 1, 0.495),
         rbinom(400, 2, 0.495)
  ))
girls

ifelse(birth_type=="single birth", rbinom(594209845, 14, 0.99999),
ifelse(birth_type=="identical twin", rbinom(238429, 28, 0.99999),
       rbinom(84325, 42, 0.99999)
))