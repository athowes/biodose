source("functions/zeroinfl_npboot_functions.R")

nsim <- 10000
ncal <- 20000
nobs <- 500

obs <- rbind(c(2, 3, 4, 5), rep(nobs, 4))

cal_data <- read.csv("data/1997_barquinero.csv")
obs_data <- read.csv("data/1997_barquinero_partial.csv")

run_fraction <- function(f) {
  qI500 <- exactify(readRDS("data/1997_qI500.Rds"), ncal)
  out <- replicate(nsim, zeroinfl_npboot(obs, f, qI500, obs_data, cal_data))
  saveRDS(out, paste0("results/zi_npboot/1997_", f, "_np_qI500_", nsim, ".Rds"))

  qI50 <- exactify(readRDS("data/1997_qI50.Rds"), ncal)
  out <- replicate(nsim, zeroinfl_npboot(obs, f, qI50, obs_data, cal_data))
  saveRDS(out, paste0("results/zi_npboot/1997_", f, "_np_qI50_", nsim, ".Rds"))

  qca1 <- exactify(readRDS("data/1997_qca1.Rds"), ncal)
  out <- replicate(nsim, zeroinfl_npboot(obs, f, qca1, obs_data, cal_data))
  saveRDS(out, paste0("results/zi_npboot/1997_", f, "_np_qca1_", nsim, ".Rds"))

  qD <- exactify(readRDS("data/1997_qD.Rds"), ncal)
  out <- replicate(nsim, zeroinfl_npboot(obs, f, qD, obs_data, cal_data))
  saveRDS(out, paste0("results/zi_npboot/1997_", f, "_np_qD_", nsim, ".Rds"))
  
  classical <- exactify(readRDS("data/classical.Rds"), ncal)
  out <- replicate(nsim, zeroinfl_npboot(obs, f, classical, obs_data, cal_data))
  saveRDS(out, paste0("results/zi_npboot/1997_", f, "_np_classical_", nsim, ".Rds"))
}

run_fraction(0.875)
run_fraction(0.750)
run_fraction(0.500)
run_fraction(0.250)
run_fraction(0.125)