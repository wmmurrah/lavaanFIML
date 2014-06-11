#**************************************************************************
# Section 4.15 multiple regression ----------------------------------------
# Author: William M. Murrah
# Description: This code replicates the section 4.15 example on the 
#              the appliedmissingdata.com website, which demonstrates
#              a regression analysis using FIML.
# Version history ------------------------------------------------------
#**************************************************************************
# 2014.06.05: file created.
#**************************************************************************

# R packages used
library(lavaan)

# Import text file into R as a data frame. You can download a zip file
# of the data from http://www.appliedmissingdata.com/book-examples.html.
# Remember to replace the file path in the 'read.table' function with 
# the path to the text file location on your computer.
employee <- read.table("data/employee.dat")

# Assign names to variables.
names(employee) <- c("id", "age", "tenure", "female", "wbeing", "jobsat", 
                     "jobperf", "turnover", "iq")

# Replace all missing values (-99) with R missing value character 'NA'.
employee[employee==-99] <- NA


# Create a regression model object.
model <- '
# Regression model. The b1 and b2 are labels
# similar to (b1) and (b2) in Mplus. These are
# needed to perform wald test.

  jobperf ~ b1*wbeing + b2*jobsat

# Variances
  wbeing ~~ wbeing
  jobsat ~~ jobsat

# Covariance/correlation
  wbeing ~~ jobsat
'

fit <- sem(model, employee, missing='fiml', meanstructure=TRUE, 
           fixed.x=FALSE)

summary(fit, fit.measures=TRUE, rsquare=TRUE, standardize=TRUE)

# Wald test is called seperately.
lavTestWald(fit,  constraints='b1 == 0
                               b2 == 0')
# Covariance coverage
inspect(fit, 'coverage')
# Missing data patterns
inspect(fit, 'patterns')
