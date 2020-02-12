
library("readxl")
nhefs <- read_excel("C:/Users/keiko/Dropbox/2020/spring/EPI289/nhefs.xlsx")
nhefs.nd <- nhefs[which(!is.na(nhefs$death)),]




# PS: Parametric estimation with many covariates
fit.para1 <- glm(qsmk ~ sex + race + age + I(age*age) + as.factor(education) + as.factor(active), 
                 data=nhefs.nd, family=binomial())
summary(fit.para1)



nhefs.nd$p.qsmk1 <- predict(fit.para1, type="response")
#alternative
nhefs$p_score <- (fit.para1$fitted.values)

head(cbind(nhefs.nd$seqn, nhefs.nd$sex, nhefs.nd$age, nhefs.nd$p.qsmk1))
summary(nhefs.nd$p.qsmk1)

## regression on continuous PS
fit.ps.cont <- glm(death~qsmk+p.qsmk1+I(p.qsmk1*p.qsmk1), data=nhefs.nd, family=binomial())
summary(fit.ps.cont)

exp(fit.ps.cont$coefficients)
exp(confint.default(fit.ps.cont))

#alternative



############
## Part 2 ##

nhefs$age50 <- ifelse(nhefs$age>=50, 1, 0)


prop.table(table(nhefs.nd$sex, nhefs.nd$age50 ))
prop.table(table(nhefs.nd$sex, nhefs.nd$age50 ,nhefs.nd$race))

#alt
nhefs$lstrat <- ifelse(nhefs$age50==0 & nhefs$sex==0 & nhefs $ race==0,1,NA)
nhefs$lstrat <- ifelse(nhefs$age50==0 & nhefs$sex==0 & nhefs $ race==1,2,nhefs$lstrat)
nhefs$lstrat <- ifelse(nhefs$age50==0 & nhefs$sex==1 & nhefs $ race==0,3,nhefs$lstrat)
nhefs$lstrat <- ifelse(nhefs$age50==0 & nhefs$sex==1 & nhefs $ race==1,4,nhefs$lstrat)
nhefs$lstrat <- ifelse(nhefs$age50==1 & nhefs$sex==0 & nhefs $ race==0,5,nhefs$lstrat)
nhefs$lstrat <- ifelse(nhefs$age50==1 & nhefs$sex==0 & nhefs $ race==1,6,nhefs$lstrat)
nhefs$lstrat <- ifelse(nhefs$age50==1 & nhefs$sex==1 & nhefs $ race==0,7,nhefs$lstrat)
nhefs$lstrat <- ifelse(nhefs$age50==1 & nhefs$sex==1 & nhefs $ race==1,8,nhefs$lstrat)

...
prop.table(table(nhefs$lstrat))
aggregate(nhefs$death, list(nhefs$qsmk, nhefs$lstrat), mean)
#

nrow(nhefs.nd[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==0,])/nrow(nhefs.nd)
nrow(nhefs.nd[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==1,])/nrow(nhefs.nd)
nrow(nhefs.nd[nhefs.nd$age50==0 & nhefs.nd$sex==1 & nhefs.nd$race==0,])/nrow(nhefs.nd)
nrow(nhefs.nd[nhefs.nd$age50==0 & nhefs.nd$sex==1 & nhefs.nd$race==1,])/nrow(nhefs.nd)
nrow(nhefs.nd[nhefs.nd$age50==1 & nhefs.nd$sex==0 & nhefs.nd$race==0,])/nrow(nhefs.nd)
nrow(nhefs.nd[nhefs.nd$age50==1 & nhefs.nd$sex==0 & nhefs.nd$race==1,])/nrow(nhefs.nd)
nrow(nhefs.nd[nhefs.nd$age50==1 & nhefs.nd$sex==1 & nhefs.nd$race==0,])/nrow(nhefs.nd)
nrow(nhefs.nd[nhefs.nd$age50==1 & nhefs.nd$sex==1 & nhefs.nd$race==1,])/nrow(nhefs.nd)
nrow(nhefs.nd$qsmk[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==0])

#A=1
summary(nhefs.nd$death[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==0 & nhefs.nd$qsmk==1])
summary(nhefs.nd$death[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==1 & nhefs.nd$qsmk==1])
summary(nhefs.nd$death[nhefs.nd$age50==0 & nhefs.nd$sex==1 & nhefs.nd$race==0 & nhefs.nd$qsmk==1])
summary(nhefs.nd$death[nhefs.nd$age50==0 & nhefs.nd$sex==1 & nhefs.nd$race==1 & nhefs.nd$qsmk==1])
summary(nhefs.nd$death[nhefs.nd$age50==1 & nhefs.nd$sex==0 & nhefs.nd$race==0 & nhefs.nd$qsmk==1])
summary(nhefs.nd$death[nhefs.nd$age50==1 & nhefs.nd$sex==0 & nhefs.nd$race==1 & nhefs.nd$qsmk==1])
summary(nhefs.nd$death[nhefs.nd$age50==1 & nhefs.nd$sex==1 & nhefs.nd$race==0 & nhefs.nd$qsmk==1])
summary(nhefs.nd$death[nhefs.nd$age50==1 & nhefs.nd$sex==1 & nhefs.nd$race==1 & nhefs.nd$qsmk==1])
#A=0
summary(nhefs.nd$death[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==0 & nhefs.nd$qsmk==0])
summary(nhefs.nd$death[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==1 & nhefs.nd$qsmk==0])
summary(nhefs.nd$death[nhefs.nd$age50==0 & nhefs.nd$sex==1 & nhefs.nd$race==0 & nhefs.nd$qsmk==0])
summary(nhefs.nd$death[nhefs.nd$age50==0 & nhefs.nd$sex==1 & nhefs.nd$race==1 & nhefs.nd$qsmk==0])
summary(nhefs.nd$death[nhefs.nd$age50==1 & nhefs.nd$sex==0 & nhefs.nd$race==0 & nhefs.nd$qsmk==0])
summary(nhefs.nd$death[nhefs.nd$age50==1 & nhefs.nd$sex==0 & nhefs.nd$race==1 & nhefs.nd$qsmk==0])
summary(nhefs.nd$death[nhefs.nd$age50==1 & nhefs.nd$sex==1 & nhefs.nd$race==0 & nhefs.nd$qsmk==0])
summary(nhefs.nd$death[nhefs.nd$age50==1 & nhefs.nd$sex==1 & nhefs.nd$race==1 & nhefs.nd$qsmk==0])
###########


# analysis with models: 2 confounders 
condfit.3 <- glm(death~qsmk*sex*age50*race, data=nhefs.nd,family=binomial(link="logit"))
summary(condfit.3)

#A=1
plogis(condfit.3, data.frame(cbind(qsmk=1, sex=0, age50=0,race=0)))
predict(condfit.3, data.frame(cbind(qsmk=1, sex=0, age50=0,race=0)))
predict(condfit.3, data.frame(cbind(qsmk=1, sex=0, age50=0,race=1)))
predict(condfit.3, data.frame(cbind(qsmk=1, sex=1, age50=0,race=0)))
predict(condfit.3, data.frame(cbind(qsmk=1, sex=1, age50=0,race=1)))
predict(condfit.3, data.frame(cbind(qsmk=1, sex=0, age50=1,race=0)))
predict(condfit.3, data.frame(cbind(qsmk=1, sex=0, age50=1,race=1)))
predict(condfit.3, data.frame(cbind(qsmk=1, sex=1, age50=1,race=0)))
predict(condfit.3, data.frame(cbind(qsmk=1, sex=1, age50=1,race=1)))
#A=0
predict(condfit.3, data.frame(cbind(qsmk=0, sex=0, age50=0, race=0)))
predict(condfit.3, data.frame(cbind(qsmk=0, sex=0, age50=0, race=1)))
predict(condfit.3, data.frame(cbind(qsmk=0, sex=1, age50=0,race=0)))
predict(condfit.3, data.frame(cbind(qsmk=0, sex=1, age50=0,race=1)))
predict(condfit.3, data.frame(cbind(qsmk=0, sex=0, age50=1,race=0)))
predict(condfit.3, data.frame(cbind(qsmk=0, sex=0, age50=1,race=1)))
predict(condfit.3, data.frame(cbind(qsmk=0, sex=1, age50=1,race=0)))
predict(condfit.3, data.frame(cbind(qsmk=0, sex=1, age50=1,race=1)))

#alternative
nhefs$interv <- -1 # 1st copy: equal to original one

interv0 <- nhefs # 2nd copy: treatment set to 0, outcome to missing
interv0$interv <- 0
interv0$qsmk <- 0
interv0$death <- NA

interv1 <- nhefs # 3rd copy: treatment set to 1, outcome to missing
interv1$interv <- 1
interv1$qsmk <- 1
interv1$death <- NA

onesample <- rbind(nhefs, interv0, interv1) # combining datasets

# linear model to estimate mean outcome conditional on treatment and confounders
# parameters are estimated using original observations only (nhefs)
# parameter estimates are used to predict mean outcome for observations with 
# treatment set to 0 (interv=0) and to 1 (interv=1)

std <- glm(death ~ qsmk * sex * race * age, data=onesample,family=binomial(link="logit") )
summary(std)   
onesample$predicted_logP <- predict(std, onesample,type="response")


onesample$predicted_logP   # predicted log odds
onesample$predicted_P <- exp(onesample$predicted_logP)
onesample$predicted_P  # predicted odds
onesample$prob <- onesample$predicted_P/(onesample$predicted_P+1)
onesample$prob

mean(onesample[which(onesample$interv==1),]$predicted_logP)-mean(onesample[which(onesample$interv==0),]$predicted_logP)

# estimate mean outcome in each of the groups interv=0, and interv=1
# this mean outcome is a weighted average of the mean outcomes in each combination 
# of values of treatment and confounders, that is, the standardized outcome
mean(onesample[which(onesample$interv==-1),]$prob)
mean(onesample[which(onesample$interv==0),]$prob)
mean(onesample[which(onesample$interv==1),]$prob)
mean(onesample[which(onesample$interv==1),]$prob)-mean(onesample[which(onesample$interv==0),]$prob)


##################################################################
# Standardization by multiple confounders using an outcome model #
##################################################################

# create a dataset with 3 copies of each subject
nhefs$interv <- -1 # 1st copy: equal to original one

interv0 <- nhefs # 2nd copy: treatment set to 0, outcome to missing
interv0$interv <- 0
interv0$qsmk <- 0
interv0$death <- NA

interv1 <- nhefs # 3rd copy: treatment set to 1, outcome to missing
interv1$interv <- 1
interv1$qsmk <- 1
interv1$death <- NA

onesample <- rbind(nhefs, interv0, interv1) # combining datasets

# linear model to estimate mean outcome conditional on treatment and confounders
# parameters are estimated using original observations only (nhefs)
# parameter estimates are used to predict mean outcome for observations with 
# treatment set to 0 (interv=0) and to 1 (interv=1)

std <- glm(death ~ qsmk + sex + race + age, data=onesample,family=binomial(link="logit") )
summary(std)   
onesample$predicted_logP <- predict(std, onesample)


onesample$predicted_logP   # predicted log odds
onesample$predicted_P <- exp(onesample$predicted_logP)
onesample$predicted_P  # predicted odds
onesample$prob <- onesample$predicted_P/(onesample$predicted_P+1)
onesample$prob

# estimate mean outcome in each of the groups interv=0, and interv=1
# this mean outcome is a weighted average of the mean outcomes in each combination 
# of values of treatment and confounders, that is, the standardized outcome
mean(onesample[which(onesample$interv==-1),]$prob)
mean(onesample[which(onesample$interv==0),]$prob)
mean(onesample[which(onesample$interv==1),]$prob)
mean(onesample[which(onesample$interv==1),]$prob)-mean(onesample[which(onesample$interv==0),]$prob)

newdata = data.frame(wt = 2.1, disp = 180)
Now we use the predict() function to calculate the predicted probability. We include the argument type=”response” in order to get our prediction.
predict(model, newdata, type="response")
0.2361081

################################
#### additional ###############
#### boot strapping   #########
################################

library("boot")


# function to calculate difference in means
standardization <- function(data, indices) {   #standardization on what?#indicies: which dataset to use#
  # create a dataset with 3 copies of each subject
  d <- data[indices,] # 1st copy: equal to original one`
  d$interv <- -1
  d0 <- d # 2nd copy: treatment set to 0, outcome to missing
  d0$interv <- 0
  d0$qsmk <- 0
  d0$wt82_71 <- NA
  d1 <- d # 3rd copy: treatment set to 1, outcome to missing
  d1$interv <- 1
  d1$qsmk <- 1
  d1$wt82_71 <- NA
  d.onesample <- rbind(d, d0, d1) # combining datasets
  
  # linear model to estimate mean outcome conditional on treatment and confounders
  # parameters are estimated using original observations only (interv= -1)
  # parameter estimates are used to predict mean outcome for observations with set 
  # treatment (interv=0 and interv=1)
  fit <- glm(wt82_71 ~ qsmk + sex + race + age + I(age*age) + 
               as.factor(education) + smokeintensity + 
               I(smokeintensity*smokeintensity) + smokeyrs + I(smokeyrs*smokeyrs) +
               as.factor(exercise) + as.factor(active) + wt71 + I(wt71*wt71), 
             data=d.onesample)
  
  d.onesample$predicted_meanY <- predict(fit, d.onesample)
  
  # estimate mean outcome in each of the groups interv=-1, interv=0, and interv=1
  return(c(mean(d.onesample$predicted_meanY[d.onesample$interv==-1]),
           mean(d.onesample$predicted_meanY[d.onesample$interv==0]),
           mean(d.onesample$predicted_meanY[d.onesample$interv==1]),
           mean(d.onesample$predicted_meanY[d.onesample$interv==1])-
             mean(d.onesample$predicted_meanY[d.onesample$interv==0])))
}

# bootstrap
results <- boot(data=nhefs, statistic=standardization, R=5)  #R: how many bootstrap, usually>500
#result: t1 original, t2 no one treated, t3 everyone treated, t4 causal


# generating confidence intervals
se <- c(sd(results$t[,1]), sd(results$t[,2]), 
        sd(results$t[,3]), sd(results$t[,4]))
mean <- c(mean(results$t[,1]), mean(results$t[,2]), 
          mean(results$t[,3]), mean(results$t[,4]))
ll <- mean - qnorm(0.975)*se
ul <- mean + qnorm(0.975)*se
# we ca also use original mean as the center of the CI

bootstrap <- data.frame(cbind(c("Observed", "No Treatment", "Treatment", 
                                "Treatment - No Treatment"), mean, se, ll, ul))
bootstrap