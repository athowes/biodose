# The 1997 dataset (with rings)
dat97 <- read.csv("data/1997_barquinero.csv")

d <- dat97$d
m <- dat97$m
s <- dat97$s
fit <- glm(s ~ m + I(m*d) + I(m*d^2) - 1,
           family = poisson(link = identity), start = c(1, 1, 1))
a <- as.numeric(fit$coefficients)
Sigma <- matrix(vcov(fit), ncol = 3)

saveRDS(a, "data/1997_a.Rds")
saveRDS(Sigma, "data/1997_Sigma.Rds")