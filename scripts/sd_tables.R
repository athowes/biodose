library(tidyverse)

# Non-parametric, full body exposure --------------------------------------
files <- list.files("results/npboot/", full.names = TRUE)
npboot <- lapply(files, readRDS)
names(npboot) <- gsub("results/npboot/1997_|.Rds", "", files)
npboot_sd <- lapply(npboot, function(item) {
    item[item < 0] <- 0 # Replace negatives with zero
    item[is.na(item)] <- 0 # Replace NAs with zero
    item <- apply(item, 1, sd)
    setNames(item, c(0, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5))
    })
npboot_sd <- as.data.frame(do.call(rbind, npboot_sd))

npboot_sd %<>% 
  rownames_to_column() %>% 
  separate(rowname, into = c("simulation", "design", "nsim"), sep = "_") %>%
  mutate(irr_frac = 1) %>%
  select(irr_frac, everything()) %>%
  gather(dose, sd, 5:15)
  
# Non-parametric, partial body exposure -----------------------------------
files <- list.files("results/zi_npboot/", full.names = TRUE)
zi_npboot <- lapply(files, readRDS)
names(zi_npboot) <- gsub("results/zi_npboot/1997_|.Rds", "", files)
zi_npboot_sd <- lapply(zi_npboot, function(item) {
    item[item < 0] <- 0 # Replace negatives with zero
    item[is.na(item)] <- 0 # Replace NAs with zero
    item <- apply(item, 1, sd)
    setNames(item, c(2, 3, 4, 5))
    })
zi_npboot_sd <- as.data.frame(do.call(rbind, zi_npboot_sd))

zi_npboot_sd %<>% 
  rownames_to_column() %>% 
  separate(rowname, 
           into = c("irr_frac", "simulation", "design", "nsim"), 
           sep = "_") %>%
  gather(dose, sd, 5:8) %>%
  arrange(desc(irr_frac))

# Parametric, full body exposure ------------------------------------------
files <- list.files("results/pboot/", full.names = TRUE)
files <- files[-6] # Remove simple
pboot <- lapply(files, readRDS)
names(pboot) <- gsub("results/pboot/1997_|.Rds", "", files)
pboot_sd <- lapply(pboot, function(item) {
  item[item < 0] <- 0 # Replace negatives with zero
  item[is.na(item)] <- 0 # Replace NAs with zero
  item <- apply(item, 1, sd)
  setNames(item, c(0, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5))
})
pboot_sd <- as.data.frame(do.call(rbind, pboot_sd))

pboot_sd %<>% 
  rownames_to_column() %>% 
  separate(rowname, into = c("simulation", "design", "nsim"), sep = "_") %>%
  mutate(irr_frac = 1) %>%
  select(irr_frac, everything()) %>%
  gather(dose, sd, 5:15)

# Parametric, partial body exposure ---------------------------------------

files <- list.files("results/zi_pboot/", full.names = TRUE)
files <- files[-26] # Remove old
zi_pboot <- lapply(files, readRDS)
names(zi_pboot) <- gsub("results/zi_pboot/1997_|.Rds", "", files)
zi_pboot_sd <- lapply(zi_pboot, function(item) {
  item[item < 0] <- 0 # Replace negatives with zero
  item[is.na(item)] <- 0 # Replace NAs with zero
  item <- apply(item, 1, sd)
  setNames(item, c(0, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 5))
})
zi_pboot_sd <- as.data.frame(do.call(rbind, zi_pboot_sd))

zi_pboot_sd %<>% 
  rownames_to_column() %>% 
  separate(rowname, 
           into = c("irr_frac", "simulation", "design", "nsim"), 
           sep = "_") %>%
  gather(dose, sd, 5:15) %>%
  arrange(desc(irr_frac))

# Output ------------------------------------------------------------------

out <- rbind(npboot_sd, zi_npboot_sd, pboot_sd, zi_pboot_sd)

write.csv(out, "results/sd_table.csv")
