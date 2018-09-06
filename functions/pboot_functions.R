library(MASS)

# Left truncated multivariate normal
mvrnorm0LT <- function(m, Sigma) {
 u <- mvrnorm(1, m, Sigma)
 while (sum(u < 0)) u <- mvrnorm(1, m, Sigma)
 return(u)
}
# m: 1xn mean vector
# Sigma: nxn variance matrix

# Helper function to go from a probability measure to an exact design
exactify <- function(design, n) {
  design[-1, ] <- round(design[-1, ] * n)
  design[-1, ncol(design)] <- (n - sum(design[-1, -ncol(design)]))
  return(design)
}
# design: 2xn first row is the design points, second row is the sample size
# n: total sample size

pboot <- function(observed, design, a, Sigma) {
  q <- function(d) {
    mvrnorm0LT(a, Sigma) %*% c(1, d, d^2)
  } # Quadratic function of d, drawing nominal values from left truncated MVN
  
  g <- function(col) {sum(replicate(col[2], rpois(1, q(col[1]))))}
  
  # The simulated observations
  m <- observed[2, ] # Vector of observation sample sizes
  s <- apply(observed, 2, g) # Vector of observation dicentric counts
  
  # The simulated calibration design
  D <- design[1, ] # Vector of design locations
  M <- design[2, ] # Vector of design sample sizes
  S <- apply(design, 2, g) # Vector of design dicentric counts
  
  # Fit a Poisson GLM to the simulated data and extract the coefficients
  fit <- glm(S ~ M + I(M*D) + I(M*D^2) - 1, 
             family = poisson(link = identity), start = a)
  h0 <- fit$coefficients[1][[1]]
  h1 <- fit$coefficients[2][[1]]
  h2 <- fit$coefficients[3][[1]]
  
  # Calculate the dose estimates  
  est <- (-h1 + sqrt(h1^2 - 4*h2*(h0 - s/m)))/(2*h2)
  return(est)
}
# observed: 2xn first row is the observation points, second row is sample size
# design: 2xn first row is the design points, second row is the sample size
# a: Nomial values of parameters, default to Barquinero et al. 1995
# Sigma: Variance matrix of a

simple_pboot <- function(observed, design, a) {
  q <- function(d) {
    a %*% c(1, d, d^2)
  } # Quadratic function of d
  
  g <- function(col) {rpois(1, col[2]*q(col[1]))}
  
  # The simulated observations
  m <- observed[2, ] # Vector of observation sample sizes
  s <- apply(observed, 2, g) # Vector of observation dicentric counts
  
  # The simulated calibration design
  D <- design[1, ] # Vector of design locations
  M <- design[2, ] # Vector of design sample sizes
  S <- apply(design, 2, g) # Vector of design dicentric counts
  
  # Fit a Poisson GLM to the simulated data and extract the coefficients
  fit <- glm(S ~ M + I(M*D) + I(M*D^2) - 1, 
             family = poisson(link = identity), start = a)
  h0 <- fit$coefficients[1][[1]]
  h1 <- fit$coefficients[2][[1]]
  h2 <- fit$coefficients[3][[1]]
  
  # Calculate the dose estimates  
  est <- (-h1 + sqrt(h1^2 - 4*h2*(h0 - s/m)))/(2*h2)
  return(est)
}
# observed: 2xn first row is the observation points, second row is sample size
# design: 2xn first row is the design points, second row is the sample size
# a: Nomial values of parameters, default to Barquinero et al. 1995