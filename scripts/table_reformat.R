library(tidyverse)
library(xtable)

sd_table <- read.csv("results/sd_table.csv")

to_paperformat <- function(frac, sim) {
  tex <- sd_table %>% 
    filter(irr_frac == frac, simulation == sim) %>%
    select(-c(irr_frac, simulation, nsim)) %>%
    spread(dose, sd) %>%
    arrange(factor(design, levels = c("classical", "I500", "qI500", "I50",
                                      "qI50", "ca1", "qca1", "D", "qD"))) %>%
    xtable(caption = paste0("Irradiated fraction = ", frac, ", Simulation type = ", sim),
           digits = 4)
  
  print(tex, include.rownames = FALSE)
}

to_paperformat(1, "p")
to_paperformat(1, "np")

to_paperformat(0.875, "p")
to_paperformat(0.875, "np")

to_paperformat(0.75, "p")
to_paperformat(0.75, "np")

to_paperformat(0.5, "p")
to_paperformat(0.5, "np")

to_paperformat(0.25, "p")
to_paperformat(0.25, "np")

to_paperformat(0.125, "p")
to_paperformat(0.125, "np")