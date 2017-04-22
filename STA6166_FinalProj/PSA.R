psa_dat = read.table("http://www.stat.ufl.edu/~athienit/STA6166/assignment3_1.txt", 
                     col.names = c('ID', 'PSA_lvl', 'Cancer_vol', 'Weight', 'Age', 
                                   'BPH', 'SVI', 'CP', 'Gleason_score'))
View(psa_dat)

#Scatterplots of PSA Level with predictors
plot(psa_dat$PSA_lvl~psa_dat$Cancer_vol, xlab='Cancer Volume', ylab='PSA Level', pch=19)
plot(psa_dat$PSA_lvl~psa_dat$Weight, xlab='Weight', ylab='PSA Level', pch=19)
plot(psa_dat$PSA_lvl~psa_dat$Age, xlab='Age', ylab='PSA Level', pch=19)
plot(psa_dat$PSA_lvl~psa_dat$BPH, xlab='BPH', ylab='PSA Level', pch=19)
plot(psa_dat$PSA_lvl~psa_dat$SVI, xlab='SVI', ylab='PSA Level', pch=19)
plot(psa_dat$PSA_lvl~psa_dat$CP, xlab='CP', ylab='PSA Level', pch=19)
plot(psa_dat$PSA_lvl~psa_dat$Gleason_score, xlab='Gleason Score', ylab='PSA Level', pch=19)

#Correlation Matrix
cor_mat = round(cor(psa_dat[c('ID', 'PSA_lvl', 'Cancer_vol', 'Weight', 'Age', 
                    'BPH', 'SVI', 'CP', 'Gleason_score')]), 3)
View(cor_mat)

source("http://www.stat.ufl.edu/~athienit/check.R")

#Full Linear Model
psa.lm.full <- lm(PSA_lvl ~ Cancer_vol + Weight + Age + BPH + SVI + CP, data = psa_dat)
check(psa.lm.full, tests=TRUE)
summary(psa.lm.full)
plot(psa.lm.full)

#Partial Quadratic Model - Derived using the correlation matrix
cancervol_svi <- psa_dat$Cancer_vol*psa_dat$SVI
cancervol_cp <- psa_dat$Cancer_vol*psa_dat$CP
svi_cp <- psa_dat$SVI*psa_dat$CP
psa.qm.partial <- lm(PSA_lvl ~ Cancer_vol + Weight + Age + BPH + SVI + CP + cancervol_svi 
                     + cancervol_cp + svi_cp, data = psa_dat)
summary(psa.qm.partial)