#**************************************************************************
# Repeated Measures -------------------------------------------------------
# Author: William M. Murrah
#**************************************************************************
# R packages used
library(lavaan)

# Import text file into R as a data frame.
antisocial <- read.table("Computer Files/Maximum Likeihood/antisocial.dat", 
                       quote="\"")

# Assign names to variables.
names(antisocial) <- c("female", "anti14", "anti15", "anti16", "anti17", 
                     "anti18", "anti19")

# Replace all missing values (-99) with R missing value character 'NA'.
antisocial[antisocial==-99] <- NA

model <- '
anti14 ~ m14*1
anti15 ~ m15*1
anti16 ~ m16*1
anti17 ~ m17*1
anti18 ~ m18*1
anti19 ~ m19*1
anti14 ~~ anti15 + anti16 + anti17 + anti18 + anti19
anti15 ~~ anti16 + anti17 + anti18 + anti19
anti16 ~~ anti17 + anti18 + anti19
anti17 ~~ anti18 + anti19
anti18 ~~ anti19
'
fit <- sem(model, antisocial, missing='fiml', meanstructure=TRUE, fixed.x=TRUE)

summary(fit, fit.measures=TRUE)

# Wald test is called seperately.
lavTestWald(fit,  constraints='m14==m15
                               m15==m16
                               m16==m17
                               m17==m18
                               m18==m19')
# Covariance coverage
inspect(fit, 'coverage')
# Missing data patterns
inspect(fit, 'patterns')
