
library("readxl")
nhefs <- read_excel("C:/Users/keiko/Dropbox/2020/spring/EPI289/nhefs.xlsx")
nhefs.nd <- nhefs[which(!is.na(nhefs$death)),]




# PS: Parametric estimation with many covariates
fit.para1 <- glm(qsmk ~ sex + race + age + I(age*age) + as.factor(education) + as.factor(active), 
                 data=nhefs.nd, family=binomial())
summary(fit.para1)

nhefs.nd$p.qsmk1 <- predict(fit.para1, type="response")
head(cbind(nhefs.nd$seqn, nhefs.nd$sex, nhefs.nd$age, nhefs.nd$p.qsmk1))
summary(nhefs.nd$p.qsmk1)

## regression on continuous PS
fit.ps.cont <- glm(death~qsmk+p.qsmk1+I(p.qsmk1*p.qsmk1), data=nhefs.nd, family=binomial())
summary(fit.ps.cont)

exp(fit.ps.cont$coefficients)
exp(confint.default(fit.ps.cont))



############
## Part 2 ##

nhefs.nd$age50 <- ifelse(nhefs.nd$age>=50, 1, 0)


prop.table(table(nhefs.nd$sex, nhefs.nd$age50 ))
prop.table(table(nhefs.nd$sex, nhefs.nd$age50 ,nhefs.nd$race))

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