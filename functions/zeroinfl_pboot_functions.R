library(MASS)
library(pscl)

# Helper function to go from a probability measure to an exact design
exactify <- function(design, n) {
  design[-1, ] <- round(design[-1, ] * n)
  design[-1, ncol(design)] <- (n - sum(design[-1, -ncol(design)]))
  return(design)
}
# design: 2xn first row is the design points, second row is the sample size
# n: total sample size

# Left truncated multivariate normal
mvrnorm0LT <- function(m, Sigma) {
  u <- mvrnorm(1, m, Sigma)
  while (sum(u < 0)) u <- mvrnorm(1, m, Sigma)
  return(u)
}
# m: 1xn mean vector
# Sigma: nxn variance matrix

zeroinfl_pboot <- function(observed, f, design, a, Sigma) {
  q <- function(d) {
    mvrnorm0LT(a, Sigma) %*% c(1, d, d^2)
  } # Quadratic function of d, drawing nominal values from left truncated MVN
  
  calc_omega <- function(d, f) {
    d0 <- runif(1, 2.7, 3.5)
    1/(exp(-d/d0)*(f/(1 - f)) + 1)
  } # Calculate w from dose (d), irradiated fraction (f), survival dose (d0)
  
  g <- function(col) {
    d <- col[1]
    m <- col[2]
    y <- replicate(m, ifelse(rbinom(1, 1, 1 - calc_omega(d, f)) == 0, 0, rpois(1, q(d))))
    fit <- zeroinfl(y ~ 1 | 1)
    lh <- exp(as.numeric(fit$coefficients$count)) # Lambda hat
  } # Calculate estimate of lambda
  
  lh <- apply(observed, 2, g) # Vector of estimated lambda hats
  
  # The simulated calibration design
  D <- design[1, ] # Vector of design locations
  M <- design[2, ] # Vector of design sample sizes
  
  h <- function(col) {sum(replicate(col[2], rpois(1, q(col[1]))))}
  
  S <- apply(design, 2, h) # Vector of design dicentric counts
  
  # Fit a Poisson GLM to the simulated data and extract the coefficients
  fit <- glm(S ~ M + I(M*D) + I(M*D^2) - 1, 
             family = poisson(link = identity), start = a)
  h0 <- fit$coefficients[1][[1]]
  h1 <- fit$coefficients[2][[1]]
  h2 <- fit$coefficients[3][[1]]
  
  # Calculate the dose estimates  
  est <- (-h1 + sqrt(h1^2 - 4*h2*(h0 - lh)))/(2*h2)
  return(est)
}