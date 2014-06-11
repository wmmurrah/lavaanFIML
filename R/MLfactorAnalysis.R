#**************************************************************************
# ML factor analysis ------------------------------------------------------
#**************************************************************************
library(lavaan)
eatingrisk <- read.table('Computer Files/Maximum Likeihood/eatingrisk.dat')

names(eatingrisk) <- c("abuse", "bmi", 
                       "bds1", "bds2", "bds3","bds4", "bds5", "bds6", "bds7",
                       "edr1", "edr2", "edr3","edr4", "edr5", "edr6")
eatingrisk[eatingrisk==-99] <- NA

model <- '
eatrisk =~ edr1 + edr2 + edr3 + edr4 + edr5 + edr6
'

fit <- cfa(model, eatingrisk, missing='fiml', estimator='MLR')
summary(fit, fit.measures=TRUE, rsquare=TRUE, standardize=TRUE )

library(semPlot)
semPaths(fit,'model', 'est', style='ram')

inspect(fit, 'patterns')
inspect(fit, 'coverage')
