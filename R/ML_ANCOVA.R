#**************************************************************************
# ML ANCOVA ---------------------------------------------------------------
# Author: William M. Murrah
#**************************************************************************
# R packages used
library(lavaan)

# Import text file into R as a data frame.
probsolve <- read.table("Computer Files/Maximum Likeihood/probsolve.dat", 
                       quote="\"")

# Assign names to variables.
names(probsolve) <- c("id", "school", "tx", "frlunch", "stanmath", 
                     "pretest", "probsolv")

# Replace all missing values (-99) with R missing value character 'NA'.
probsolve[probsolve==-99] <- NA
Mean <- function(x) mean(x, na.rm=T)
attach(probsolve)
probsolve$frlunch.c <- frlunch - Mean(frlunch)
probsolve$stanmath.c <- stanmath - Mean(stanmath)
probsolve$pretest.c <- pretest - Mean(pretest)
# Create regression model object
model <- '
# Regression model. 
probsolv ~ tx + pretest.c + frlunch.c + stanmath.c
'

fit <- sem(model, probsolve, missing='fiml', meanstructure=TRUE, 
           fixed.x=FALSE)

summary(fit, fit.measures=TRUE, rsquare=TRUE, standardize=TRUE)

# Wald test is called seperately.
lavTestWald(fit,  constraints='b1 == 0
                               b2 == 0')
# Covariance coverage
inspect(fit, 'coverage')
# Missing data patterns
inspect(fit, 'patterns')



# Descriptive model -------------------------------------------------------

descmod <- '
probsolv ~ 1
frlunch ~ 1
stanmath ~ 1
pretest ~ 1
'
descfit <- sem(descmod, probsolve, meanstructure=TRUE, missing='fiml')
summary(descfit, fit.measures=TRUE, standardize=TRUE)
