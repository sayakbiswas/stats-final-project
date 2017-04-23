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
psa.lm.full <- lm(PSA_lvl ~ Cancer_vol + Weight + Age + BPH + SVI + CP + Gleason_score, 
                  data = psa_dat)
summary(psa.lm.full)
anova(psa.lm.full)
AIC(psa.lm.full)
plot(psa.lm.full)
check(psa.lm.full, tests=TRUE)

#Transform Model
library(car)
bc2=powerTransform(psa.lm.full)
summary(bc2)
PSA_lvlT = bcPower(psa_dat$PSA_lvl, 0.1004)
psa.lm.full2 <- lm(PSA_lvlT ~ Cancer_vol + Weight + Age + BPH + SVI + CP + Gleason_score, 
                   data = psa_dat)
check(psa.lm.full2, tests=TRUE)
summary(psa.lm.full2)

#Automated Model Reduction
stepT(psa.lm.full2,alpha.rem=0.2,direction="backward")
psa.lm.red = lm(PSA_lvlT ~ Cancer_vol + BPH + SVI + Gleason_score, data = psa_dat)
summary(psa.lm.red)
anova(psa.lm.red)
AIC(psa.lm.red)

#Interaction
scatterplot(PSA_lvlT~Cancer_vol|SVI,smooth=FALSE,reg.line=lm,data=psa_dat)
scatterplot(PSA_lvlT~BPH|SVI,smooth=FALSE,reg.line=lm,data=psa_dat)
scatterplot(PSA_lvlT~Gleason_score|SVI,smooth=FALSE,reg.line=lm,data=psa_dat)

#Interaction Model - Added interaction terms
cancervol <- psa_dat$Cancer_vol
svi <- psa_dat$SVI
bph <- psa_dat$BPH
gscore <- psa_dat$Gleason_score
psa.model <- lm(PSA_lvlT ~ cancervol + bph + svi + gscore + cancervol:svi + bph:svi + gscore:svi,
                data = psa_dat)
check(psa.model, tests = TRUE)
summary(psa.model)
anova(psa.model)
AIC(psa.model)

#Automated Model Reduction
stepT(psa.model,alpha.rem=0.2,direction="backward")
psa.model.red = lm(PSA_lvlT ~ cancervol + bph + gscore + cancervol:svi + bph:svi + gscore:svi
                   , data = psa_dat)
summary(psa.model.red)
anova(psa.model.red)
AIC(psa.model.red)


#Prediction
newdata = data.frame(cancervol=4.2633, Weight=22.783, Age=68, bph=1.35, svi=0, CP=0, 
                     gscore=6)
predict(psa.model.red, newdata, interval='prediction', level=0.90)