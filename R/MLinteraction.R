#***********************************************************************
# Regression with interaction - continuous moderator -------------------
# Author: William M. Murrah
# Description: This code replicates regression example of an interaction
#              with a continuous moderator found on the website 
#              http://www.appliedmissingdata.com/.
# Version history ------------------------------------------------------
# 2014.06.16: code created
#***********************************************************************
library(lavaan)
consc <- read.table('data/conscientiousness.dat')

names(consc) <- c("agree", "consc", "jobperf")
consc[consc==-99] <- NA

# Create interaction variable.
consc$interact <- consc$agree*consc$consc

model <- '
# Model mean of moderator with label
agree ~ meanm*1

# Variance of moderator with label
agree ~~ varm*agree

# Covariance of predictors and interaction
consc ~~ agree + interact
agree ~~ interact

# Regression Model
jobperf ~ Bf*consc + Bm*agree + Bint*interact

# Calculate slopes of moderator at different values
slopelow := Bf + Bint*(meanm - sqrt(varm))
slopemed := Bf + Bint*meanm   		
slopehi  := Bf + Bint*(meanm + sqrt(varm))
'

fit <- cfa(model, consc, missing='fiml', estimator='MLR', fixed.x=FALSE, 
           meanstructure=TRUE)
summary(fit, fit.measures=TRUE, rsquare=TRUE, standardize=TRUE )

library(semPlot)
semPaths(fit,'model', 'est', style='ram')

inspect(fit, 'patterns')
inspect(fit, 'coverage')
