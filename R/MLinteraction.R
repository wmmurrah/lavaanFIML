#**************************************************************************
# interaction -------------------------------------------------------------
# William Murrah
#**************************************************************************
library(lavaan)
consc <- read.table('Computer Files/Maximum Likeihood/conscientiousness.dat')

names(consc) <- c("agree", "consc", "jobperf")
consc[consc==-99] <- NA
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
