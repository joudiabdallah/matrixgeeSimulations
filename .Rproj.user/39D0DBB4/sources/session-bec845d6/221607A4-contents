### Setup for different Simulation settings: Setting 1 and Setting 2

## Sample sizes
N_small <- 50
N_medium <- 100
N_large <- 500

## Matrix dimensions
rows_small <- 15
rows_large <- 100
cols_small <- 5
cols_large <- 15

## Correlation parameters
rho_low <- 0.3     # for rows
rho_high <- 0.8

phi_low <- 0.3     # for columns
phi_high <- 0.8

## Step value for constructing a_R vector
step <- 0.1

## Row-intercept vectors (a_R)
a_R_const <- function(r, value = 1.5) matrix(value, nrow = r, ncol = 1)
a_R_linear <- function(r, step = 0.1) matrix(seq(0, by = step, length.out = r), ncol = 1)


# Reproduce the results
set.seed(123)

#Covariates parameters dimension 
q <- 10                           #fixed across all simulations

#Covariates parameters 
B <- matrix(rnorm(10), nrow = q, ncol = 1)
B

# Covariates for different sample size: vector of length sample size x q
generate_covariates <- function(sample_size, q, min = -1, max = 3) {
  runif(sample_size * q, min, max)
}

# covariates generated
p_50 <- generate_covariates(50, q)
p_100 <- generate_covariates(100, q)
p_500 <- generate_covariates(500, q)





