source("http://www.stat.ufl.edu/~athienit/check.R")
source("http://www.stat.ufl.edu/~athienit/Bonf.R")
solvents = data.frame(sorption_rate=c(1.06, 0.79, 0.82, 0.89, 1.05, 0.95, 0.65, 1.15, 1.12, 1.58, 
                                      1.45, 0.57, 1.16, 1.12, 0.91, 0.83, 0.43, 0.29, 0.06, 0.44, 
                                      0.55, 0.61, 0.43, 0.51, 0.10, 0.53, 0.34, 0.06, 0.09, 0.17, 
                                      0.17, 0.60), 
                      solvent=factor(rep(c('Aromatics', 'Chloroalkanes', 'Esters'),c(9,8,15))))
View(solvents)
means = tapply(solvents$sorption_rate, solvents$solvent, mean)
means
attach(solvents)

#Analysis of Variance Table
solvent.model = aov(sorption_rate~solvent)
summary(solvent.model)
check(solvent.model, tests = TRUE)

#Bonferroni method
Bonf(sorption_rate~solvent, data=solvents)

#Tukey's HSD method
solvent.Tukey = TukeyHSD(solvent.model, "solvent")
print(solvent.Tukey)
