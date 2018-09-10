source("functions/zeroinfl_pboot_functions.R")

nsim <- 10000
ncal <- 20000
nobs <- 500

a <- readRDS("data/1997_a.Rds")
Sigma <- readRDS("data/1997_Sigma.Rds")

obs <- rbind(c(0, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5), rep(nobs, 11))

run_fraction <- function(f) {
  I500 <- exactify(readRDS("data/1997_I500.Rds"), ncal)
  out <- replicate(nsim, zeroinfl_pboot(obs, f, I500, a, Sigma))
  saveRDS(out, paste0("results/zi_pboot/1997_", f, "_p_I500_", nsim, ".Rds"))
  
  I50 <- exactify(readRDS("data/1997_I50.Rds"), ncal)
  out <- replicate(nsim, zeroinfl_pboot(obs, f, I50, a, Sigma))
  saveRDS(out, paste0("results/zi_pboot/1997_", f, "_p_I50_", nsim, ".Rds"))
  
  ca1 <- exactify(readRDS("data/1997_ca1.Rds"), ncal)
  out <- replicate(nsim, zeroinfl_pboot(obs, f, ca1, a, Sigma))
  saveRDS(out, paste0("results/zi_pboot/1997_", f, "_p_ca1_", nsim, ".Rds"))
  
  D <- exactify(readRDS("data/1997_D.Rds"), ncal)
  out <- replicate(nsim, zeroinfl_pboot(obs, f, D, a, Sigma))
  saveRDS(out, paste0("results/zi_pboot/1997_", f, "_p_D_", nsim, ".Rds"))
  
  classical <- exactify(readRDS("data/classical.Rds"), ncal)
  out <- replicate(nsim, zeroinfl_pboot(obs, f, classical, a, Sigma))
  saveRDS(out, paste0("results/zi_pboot/1997_", f, "_p_classical_", nsim, ".Rds"))
}

run_fraction(0.875)
run_fraction(0.750)
run_fraction(0.500)
run_fraction(0.250)
run_fraction(0.125)