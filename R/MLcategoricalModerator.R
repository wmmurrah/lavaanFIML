#***********************************************************************
# Regression with categorical moderator --------------------------------
# Author: William M. Murrah
# Description: This code replicates regression example of an catagorical
#              moderator found on the website 
#              http://www.appliedmissingdata.com/.
# Version history ------------------------------------------------------
# 2014.07.01: code created
#***********************************************************************
library(lavaan)
employee <- read.table("data/employee.dat")

# Assign names to variables.
names(employee) <- c("id", "age", "tenure", "female", "wbeing", "jobsat", 
                     "jobperf", "turnover", "iq")

# Replace all missing values (-99) with R missing value character 'NA'.
employee[employee==-99] <- NA

# Create centered continuous predictor and interaction term.
# 6.288 is the mean of wbeing obtained using FIML. See Descriptive 
# Statistics and Correlation tutorial.
employee$c.wbeing <- employee$wbeing - 6.288
employee$interact <- employee$c.wbeing * employee$female
employee$interact[employee$female==0 & is.na(employee$interact)] <- 0
model <- '
# Regression Model
jobperf ~ c.wbeing
'

fit <- sem(model, employee, missing='fiml', fixed.x=FALSE, 
           meanstructure=TRUE, group='female')
summary(fit, fit.measures=TRUE, rsquare=TRUE, standardize=TRUE )

library(semPlot)
semPaths(fit,'model', 'est', style='ram')

inspect(fit, 'patterns')
inspect(fit, 'coverage')


# interaction -------------------------------------------------------------


imodel <- '
jobperf ~ c.wbeing + female + interact
'

ifit <- sem(imodel, employee, missing='fiml', fixed.x=FALSE, 
           meanstructure=TRUE)
summary(ifit, rsquare=TRUE, standardize=T )
