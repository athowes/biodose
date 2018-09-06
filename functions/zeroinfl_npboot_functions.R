library(pscl)

# Helper function to go from a probability measure to an exact design
exactify <- function(design, n) {
  design[-1, ] <- round(design[-1, ] * n)
  design[-1, ncol(design)] <- (n - sum(design[-1, -ncol(design)]))
  return(design)
}
# design: 2xn first row is the design points, second row is the sample size
# n: total sample size

# Extracts a row of cells to give a vector of dicentric counts
row_extract6 <- function(r, dat) {
  return(
    c(rep(0, dat[r, 3]), rep(1, dat[r, 4]), rep(2, dat[r, 5]), 
      rep(3, dat[r, 6]), rep(4, dat[r, 7]), rep(5, dat[r, 8]),
      rep(6, dat[r, 9]))
  )
}
row_extract5 <- function(r, dat) {
  return(
    c(rep(0, dat[r, 2]), rep(1, dat[r, 3]), rep(2, dat[r, 4]), 
      rep(3, dat[r, 5]), rep(4, dat[r, 6]), rep(5, dat[r, 7]))
  )
} 

# Resamples with replacement each row of objects like dat
resample6 <- function(dat) {
  for (r in 1:nrow(dat)) {
    cells <- row_extract6(r, dat)
    cells2 <- sample(cells, dat[r, 10], replace = TRUE)
    dat[r, 3:9] <- table(factor(cells2, levels = 0:6))
    dat[r, 11] <- sum(cells2)
  }
  return(dat)
}
resample5 <- function(dat) {
  for (r in 1:nrow(dat)) {
    cells <- row_extract5(r, dat)
    cells2 <- sample(cells, dat[r, "m"], replace = TRUE)
    s2 <- sum(cells2)
    dat[r, 2:7] <- table(factor(cells2, levels = 0:5))
    dat[r, 9] <- s2 
  }
  return(dat)
}

# Produces a simulated dose estimate via non-parametric bootstrap
zeroinfl_npboot <- function(observed, f, design, obs_data, cal_data) {
  obs_data <- obs_data[which(obs_data$irr_frac == f), ]
  
  # The simulated observations
  d <- observed[1, ] # Vector of design locations, the doses
  m <- observed[2, ] # Vector of sample sizes, the number of cells analyzed
  measurement <- obs_data[which(obs_data$d %in% d),]
  measurement[ ,"m"] <- m
  measurement <- resample6(measurement)
  
  g <- function(r) {
    y <- c(rep(0, r[3]), rep(1, r[4]), rep(2, r[5]), rep(3, r[6]), 
           rep(4, r[7]), rep(5, r[8]), rep(6, r[9]))
    fit <- zeroinfl(y ~ 1 | 1)
    lh <- exp(as.numeric(fit$coefficients$count)) # Lamda hat
    return(lh)
  } # Calculate estimate of lambda
  
  lh <- as.numeric(apply(measurement, 1, g))
  
  # The simulated calibration design
  D <- design[1,] # Vector of design locations, the doses
  M <- design[2,] # Vector of sample sizes, the number of cells analyzed
  calibration <- cal_data[which(cal_data$d %in% D),]
  calibration[ ,"m"] <- M
  S <- resample5(calibration)$s
  
  # Fit a Poisson GLM to simulated data and extract coefficients
  fit <- glm(S ~ M + I(M*D) + I(M*D^2) - 1, 
             family = poisson(link = identity), start = c(1, 1, 1))
  h0 <- fit$coefficients[1][[1]]
  h1 <- fit$coefficients[2][[1]]
  h2 <- fit$coefficients[3][[1]]
  
  est <- (-h1 + sqrt(h1^2 - 4*h2*(h0 - lh)))/(2*h2) # Dose estimate
  return(est)
}
# observed: 2xn first row is observation points, second row is sample size
# design: 2xn first row is design points, second row is sample size
# dataset: e.g. 1995_barquinero