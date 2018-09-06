source("functions/npboot_functions.R")

nsim <- 10000
ncal <- 20000
nobs <- 500

obs <- rbind(c(0, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5), rep(nobs, 11))

dataset <- read.csv("data/1997_barquinero.csv")

qI500 <- exactify(readRDS("data/1997_qI500.Rds"), ncal)
out <- replicate(nsim, npboot(obs, qI500, dataset))
saveRDS(out, paste0("results/npboot/1997_np_qI500_", nsim, ".Rds"))

qI50 <- exactify(readRDS("data/1997_qI50.Rds"), ncal)
out <- replicate(nsim, npboot(obs, qI50, dataset))
saveRDS(out, paste0("results/npboot/1997_np_qI50_", nsim, ".Rds"))

qca1 <- exactify(readRDS("data/1997_qca1.Rds"), ncal)
out <- replicate(nsim, npboot(obs, qca1, dataset))
saveRDS(out, paste0("results/npboot/1997_np_qca1_", nsim, ".Rds"))

qD <- exactify(readRDS("data/1997_qD.Rds"), ncal)
out <- replicate(nsim, npboot(obs, qD, dataset))
saveRDS(out, paste0("results/npboot/1997_np_qD_", nsim, ".Rds"))

classical <- exactify(readRDS("data/classical.Rds"), ncal)
out <- replicate(nsim, npboot(obs, classical, dataset))
saveRDS(out, paste0("results/npboot/1997_np_classical_", nsim, ".Rds"))