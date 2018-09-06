# Classical design
classical <- rbind(c(0, 0.1, 0.25, 0.5, 0.75, 1,1.5, 2, 3, 4, 5),
                   c(0.2738, 0.2739, 0.11, 0.1096, 0.1003, 0.064, 0.0308, 0.0182, 0.0106, 0.0056, 0.0032))
saveRDS(classical, "data/classical.Rds")

# Barquinero et al. 1995 designs ------------------------------------------

# # Modified I-optimal, N = 500
# I500 <- rbind(c(0, 0.92, 5),
#               c(0.1208, 0.4962, 0.383))
# saveRDS(I500, "data/1995_I500.Rds")
# 
# # Modified I-optimal, N = 50
# I50 <- rbind(c(0, 1.01, 5),
#              c(0.2488, 0.4962, 0.255))
# saveRDS(I500, "data/1995_I50.Rds")
# 
# # Quasi modifed I-optimal, N = 500
# qI500 <- rbind(c(0, 1, 5),
#                c(0.1207437, 0.4995996, 0.3796566))
# saveRDS(qI500, "data/1995_qI500.Rds")
# 
# # Quasi modifed I-optimal, N = 50
# qI50 <- rbind(c(0, 1, 5),
#               c(0.2489013, 0.4163110, 0.3347877))
# saveRDS(qI50, "data/1995_qI50.Rds")

# Barquinero et al. 1997 designs ------------------------------------------

# Modified I-optimal, N = 500
I500 <- rbind(c(0, 1.228257, 5),
              c(0.05448417, 0.54630523, 0.39921060))
saveRDS(I500, "data/1997_I500.Rds")

# Modified I-optimal, N = 50
I50 <- rbind(c(0, 1.252061, 5),
             c(0.07106817, 0.52673025, 0.40220158))
saveRDS(I50, "data/1997_I50.Rds")

# c-alpha1-optimal
ca1 <- rbind(c(0, 0.8899079, 5),
             c(0.09603236, 0.79798720, 0.10598044))
saveRDS(ca1, "data/1997_ca1.Rds")

# D-optimal
D <- rbind(c(0, 1.021681, 5),
           c(1/3, 1/3, 1/3))
saveRDS(D, "data/1997_D.Rds")

# Quasi modified I-optimal, N = 500
qI500 <- rbind(c(0, 1, 5),
              c(0.05974919, 0.53438901, 0.4058618))
saveRDS(qI500, "data/1997_qI500.Rds")

# Quasi modified I-optimal, N = 50
qI50 <- rbind(c(0, 1, 5),
             c(0.08362495, 0.51003782, 0.4063372))
saveRDS(qI50, "data/1997_qI50.Rds")

# Quasi c-alpha1-optimal
qca1 <- rbind(c(0, 0.75, 5),
             c(0.09801916, 0.80718765, 0.09479319))
saveRDS(qca1, "data/1997_qca1.Rds")

# Quasi D-optimal
qD <- rbind(c(0, 1, 5),
           c(1/3, 1/3, 1/3))
saveRDS(qD, "data/1997_qD.Rds")