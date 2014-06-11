#**************************************************************************
# Repeated Measures -------------------------------------------------------
# Author: William M. Murrah
#**************************************************************************
# R packages used
library(lavaan)

# Import text file into R as a data frame.
eatingrisk <- read.table("Computer Files/Maximum Likeihood/eatingrisk.dat", 
                       quote="\"")

# Assign names to variables.
names(eatingrisk) <- c('abuse', 'bmi', paste0('bds',1:7), 
                       paste0('edr',1:6))

# Replace all missing values (-99) with R missing value character 'NA'.
eatingrisk[eatingrisk==-99] <- NA
attach(eatingrisk)
eatingrisk$bodydis <- bds1 + bds2 + bds3 + bds4 + bds5 + bds6 + bds7
eatingrisk$eatrisk<-  edr1 + edr2 + edr3 + edr4 + edr5 + edr6 

model <- '
eatrisk ~ abuse+bodydis
abuse   ~~ bds2+bds3+bds4+bds5+bds6+bds7+edr1+edr2+edr3+edr4+edr5 
bodydis ~~ bds2+bds3+bds4+bds5+bds6+bds7+edr1+edr2+edr3+edr4+edr5  
eatrisk ~~ bds2+bds3+bds4+bds5+bds6+bds7+edr1+edr2+edr3+edr4+edr5 
bds2    ~~ bds3+bds4+bds5+bds6+bds7+edr1+edr2+edr3+edr4+edr5
bds3    ~~ bds4+bds5+bds6+bds7+edr1+edr2+edr3+edr4+edr5
bds4    ~~ bds5+bds6+bds7+edr1+edr2+edr3+edr4+edr5
bds5    ~~ bds6+bds7+edr1+edr2+edr3+edr4+edr5
bds6    ~~ bds7+edr1+edr2+edr3+edr4+edr5
bds7    ~~ edr1+edr2+edr3+edr4+edr5
edr1    ~~ edr2+edr3+edr4+edr5
edr2    ~~ edr3+edr4+edr5
edr3    ~~ edr4+edr5
edr4    ~~ edr5
abuse   ~~ bodydis # Corrrelations between predictors. 
                   # This is different from Mplus!
'
fit <- sem(model, eatingrisk, missing='fiml', meanstructure=TRUE, 
           fixed.x=FALSE)

summary(fit, fit.measures=TRUE, standardize=TRUE, rsquare=TRUE)

# Covariance coverage
inspect(fit, 'coverage')
# Missing data patterns
inspect(fit, 'patterns')
