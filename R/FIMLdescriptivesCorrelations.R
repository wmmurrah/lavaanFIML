#**************************************************************************
# section 4.14 Summary Statistics -----------------------------------------
# Author: William M. Murrah
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

# Create descriptive model object
model <- '
# means
age      ~ 1
tenure   ~ 1
female   ~ 1
wbeing   ~ 1
jobsat   ~ 1
jobperf  ~ 1
turnover ~ 1
iq       ~ 1

# variances
age      ~~ age
tenure   ~~ tenure
female   ~~ female
wbeing   ~~ wbeing
jobsat   ~~ jobsat
jobperf  ~~ jobperf
turnover ~~ turnover
iq       ~~ iq

# covariances/correlations
age      ~~ tenure + female + wbeing + jobsat + jobperf + turnover + iq
tenure   ~~ female + wbeing + jobsat + jobperf + turnover + iq
female   ~~ wbeing + jobsat + jobperf + turnover + iq
wbeing   ~~ jobsat + jobperf + turnover + iq
jobsat   ~~ jobperf + turnover + iq
jobperf  ~~ turnover + iq
turnover ~~ iq
'

fit <- sem(model, employee, missing='fiml')

summary(fit, fit.measures=TRUE, standardize=TRUE)

# Get missing data patterns and covariance coverage similar
# to that found in Mplus output.
inspect(fit, 'patterns')
inspect(fit, 'coverage')
