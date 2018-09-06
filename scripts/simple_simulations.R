# Suggestion of  Jesus Lopez Fidalgo
# No sampling of nominal values from MVN

library(tidyverse)
library(xtable)

# Helper function to go from a probability measure to an exact design
exactify <- function(design, n) {
  design[-1, ] <- round(design[-1, ] * n)
  design[-1, ncol(design)] <- (n - sum(design[-1, -ncol(design)]))
  return(design)
}
# design: 2xn first row is the design points, second row is the sample size
# n: total sample size

pboot <- function(observed, design, a) {
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

nsim <- 10000
ncal <- 20000
nobs <- 500

a <- readRDS("data/1997_a.Rds")

obs <- rbind(c(0, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5), rep(nobs, 11))

I500 <- exactify(readRDS("data/1997_I500.Rds"), ncal)
out <- replicate(nsim, pboot(obs, I500, a))
saveRDS(out, paste0("results/pboot/simple/1997_ps_I500_", nsim, ".Rds"))

I50 <- exactify(readRDS("data/1997_I50.Rds"), ncal)
out <- replicate(nsim, pboot(obs, I50, a))
saveRDS(out, paste0("results/pboot/simple/1997_ps_I50_", nsim, ".Rds"))

ca1 <- exactify(readRDS("data/1997_ca1.Rds"), ncal)
out <- replicate(nsim, pboot(obs, ca1, a))
saveRDS(out, paste0("results/pboot/simple/1997_ps_ca1_", nsim, ".Rds"))

D <- exactify(readRDS("data/1997_D.Rds"), ncal)
out <- replicate(nsim, pboot(obs, D, a))
saveRDS(out, paste0("results/pboot/simple/1997_ps_D_", nsim, ".Rds"))

classical <- exactify(readRDS("data/classical.Rds"), ncal)
out <- replicate(nsim, pboot(obs, classical, a))
saveRDS(out, paste0("results/pboot/simple/1997_p_s_classical_", nsim, ".Rds"))

files <- list.files("results/pboot/simple/", full.names = TRUE)
pboot_s <- lapply(files, readRDS)
names(pboot_s) <- gsub("results/pboot/simple/1997_|.Rds", "", files)
pboot_s_sd <- lapply(pboot_s, function(item) {
  item[item < 0] <- 0 # Replace negatives with zero
  item[is.na(item)] <- 0 # Replace NAs with zero
  item <- apply(item, 1, sd)
  setNames(item, c(0, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5))
})
pboot_s_sd <- as.data.frame(do.call(rbind, pboot_s_sd))

pboot_s_sd %<>% 
  rownames_to_column() %>% 
  separate(rowname, into = c("simulation", "design", "nsim"), sep = "_") %>%
  mutate(irr_frac = 1) %>%
  select(irr_frac, everything()) %>%
  gather(dose, sd, 5:15)

write.csv(pboot_s_sd, "results/sd_table_simple.csv")

pboot_s_sd$design <- factor(pboot_s_sd$design, 
                            levels = c("classical", "I500", "qI500", "I50", "qI50",
                                       "ca1", "qca1", "D", "qD"))

# Plot
pboot_s_sd %>%
  ggplot(aes(x = dose, y = sd, col = design, group = design)) +
  geom_line(size = 1, alpha = 0.5) +
  geom_point(size = 3, shape = 1, alpha = 0.9) + 
  labs(x = "Dose estimate", y = "Standard Deviation",
       col = "Experimental \nDesign") +
  theme_minimal()

# Much quicker, almost the same results

# Table for paper
tex <- pboot_s_sd %>% 
  select(-c(irr_frac, simulation, nsim)) %>%
  spread(dose, sd) %>%
  arrange(factor(design, levels = c("classical", "I500", "qI500", "I50",
                                    "qI50", "ca1", "qca1", "D", "qD"))) %>%
  xtable(caption = "Irradiated fraction = 1, Simulation type = ps",
         digits = 4)

print(tex, include.rownames = FALSE)