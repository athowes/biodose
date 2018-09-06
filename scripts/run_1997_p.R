source("functions/pboot_functions.R")

nsim <- 2500
ncal <- 20000
nobs <- 500

a <- readRDS("data/1997_a.Rds")
Sigma <- readRDS("data/1997_Sigma.Rds")

obs <- rbind(c(0, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5), rep(nobs, 11))

I500 <- exactify(readRDS("data/1997_I500.Rds"), ncal)
out <- replicate(nsim, pboot(obs, I500, a, Sigma))
saveRDS(out, paste0("results/pboot/1997_p_I500_", nsim, ".Rds"))

I50 <- exactify(readRDS("data/1997_I50.Rds"), ncal)
out <- replicate(nsim, pboot(obs, I50, a, Sigma))
saveRDS(out, paste0("results/pboot/1997_p_I50_", nsim, ".Rds"))

ca1 <- exactify(readRDS("data/1997_ca1.Rds"), ncal)
out <- replicate(nsim, pboot(obs, ca1, a, Sigma))
saveRDS(out, paste0("results/pboot/1997_p_ca1_", nsim, ".Rds"))

D <- exactify(readRDS("data/1997_D.Rds"), ncal)
out <- replicate(nsim, pboot(obs, D, a, Sigma))
saveRDS(out, paste0("results/pboot/1997_p_D_", nsim, ".Rds"))

classical <- exactify(readRDS("data/classical.Rds"), ncal)
out <- replicate(nsim, pboot(obs, classical, a, Sigma))
saveRDS(out, paste0("results/pboot/1997_p_classical_", nsim, ".Rds"))