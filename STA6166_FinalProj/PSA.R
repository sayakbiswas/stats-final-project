source("http://www.stat.ufl.edu/~athienit/check.R")
source("http://www.stat.ufl.edu/~athienit/stepT.R")
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

#Full Linear Model
psa.lm.full <- lm(PSA_lvl ~ Cancer_vol + Weight + Age + BPH + SVI + CP, data = psa_dat)
check(psa.lm.full, tests=TRUE)
summary(psa.lm.full)
anova(psa.lm.full)
AIC(psa.lm.full)
plot(psa.lm.full)
stepT(psa.lm.full,alpha.rem=0.2,direction="backward")
psa.lm.red = lm(PSA_lvl ~ Cancer_vol + SVI, data = psa_dat)
summary(psa.lm.red)
anova(psa.lm.red)
AIC(psa.lm.red)

#Partial Quadratic Model - Derived using the correlation matrix
cancervol_svi <- psa_dat$Cancer_vol*psa_dat$SVI
cancervol_cp <- psa_dat$Cancer_vol*psa_dat$CP
svi_cp <- psa_dat$SVI*psa_dat$CP
psa.qm.partial <- lm(PSA_lvl ~ Cancer_vol + Weight + Age + BPH + SVI + CP + cancervol_svi 
                     + cancervol_cp + svi_cp, data = psa_dat)
summary(psa.qm.partial)
anova(psa.qm.partial)
AIC(psa.qm.partial)
plot(psa.qm.partial)
stepT(psa.qm.partial,alpha.rem=0.2,direction="backward")
psa.qm.partial_red = lm(formula = PSA_lvl ~ Cancer_vol + BPH + SVI + CP + cancervol_svi + 
                            cancervol_cp, data = psa_dat)
summary(psa.qm.partial_red)
anova(psa.qm.partial_red)
AIC(psa.qm.partial_red)

#Prediction
newdata = data.frame(Cancer_vol=4.2633, Weight=22.783, Age=68, BPH=1.35, SVI=0, CP=0, 
                     Gleason_score=6, cancervol_svi=0, cancervol_cp=0)
predict(psa.qm.partial_red, newdata, interval='prediction', level=0.90)
