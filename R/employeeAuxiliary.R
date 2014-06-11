# employeeAuxiliary.R ---------------------------------------------------

employee <- read.table("Computer Files/Maximum Likeihood/employee.dat", 
                       quote="\"")
names(employee) <- c("id", "age", "tenure", "female", "wbeing", "jobsat", 
                     "jobperf", "turnover", "iq")
employee[employee==-99] <- NA
library(psych)
describe(employee)
library(lavaan)

model <- '
jobperf ~ b1*wbeing + b2*jobsat
wbeing ~~ jobsat
wbeing ~~ turnover + iq
jobsat ~~ turnover + iq
jobperf ~~ turnover + iq
turnover ~~ iq
'
fit <- sem(model, employee, missing='fiml', fixed.x=FALSE, meanstructure=TRUE)
summary(fit, fit.measures=TRUE, rsquare=T, standardize=T)

lavTestWald(fit, 
            'b1 == 0
             b2 == 0')
# semTools auxiliary function ---------------------------------------------

library(semTools)

model2 <- '
jobperf ~ wbeing + jobsat'
aux <- c('turnover', 'iq')

fit2 <- sem(model2, employee, missing='fiml', meanstructure=TRUE, fixed.x=FALSE)
summary(fit2, fit.measures=TRUE, rsquare=TRUE, standardize=TRUE)
pt <- parTable(fit2)
auxfit <- sem.auxiliary(model=fit2, aux=aux, data=employee)
summary(auxfit, fit.measures=TRUE, rsquare=TRUE, standardize=TRUE)
