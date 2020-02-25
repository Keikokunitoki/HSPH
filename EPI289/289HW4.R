library("readxl")
library("geepack")
nhefs <- read_excel("C:/Users/keiko/Dropbox/2020/spring/EPI289/nhefs.xlsx")


# estimation of denominator of treatment weights
denom.fit <- glm(qsmk ~ sex + race + age  + as.factor(weakheart) + smokeintensity + as.factor(asthma) + as.factor(bronch),
                  family = binomial(), data = nhefs)
summary(denom.fit)

pd.qsmk <- predict(denom.fit, type = "response")

# estimation of numerator of treatment weights
numer.fit <- glm(qsmk~1, family = binomial(), data = nhefs)
summary(numer.fit)
pn.qsmk <- predict(numer.fit, type = "response")

nhefs$sw <- ifelse(nhefs$qsmk == 0, ((1-pn.qsmk)/(1-pd.qsmk)),
                     (pn.qsmk/pd.qsmk))
summary(nhefs$sw)

# saturated marginal structural model, stabilized weights
msm.sw <- geeglm(death ~ qsmk, data=nhefs, weights=sw, id=seqn,
                 corstr="independence")
summary(msm.sw)

beta <- coef(msm.sw)
SE <- coef(summary(msm.sw))[,2]
lcl <- beta-qnorm(0.975)*SE 
ucl <- beta+qnorm(0.975)*SE
cbind(beta, lcl, ucl)

#####
library("boot")
stabwei <- function(data, indices) {
 d <-data[indices,]
   denom.fit <- glm(qsmk ~ sex + race + age  + as.factor(weakheart) + smokeintensity + as.factor(asthma) + as.factor(bronch),
                   family = binomial(), data = d)
  summary(denom.fit)
  
  pd.qsmk <- predict(denom.fit, type = "response")
  
  # estimation of numerator of treatment weights
  numer.fit <- glm(qsmk~1, family = binomial(), data = d)
  summary(numer.fit)
  pn.qsmk <- predict(numer.fit, type = "response")
  
  nhefs$sw <- ifelse(nhefs$qsmk == 0, ((1-pn.qsmk)/(1-pd.qsmk)),
                     (pn.qsmk/pd.qsmk))
  summary(d$sw)
  
  # saturated marginal structural model, stabilized weights
  msm.sw <- geeglm(death ~ qsmk, data=d, weights=sw, id=seqn,
                   corstr="independence")
  

# estimate mean outcome in each of the groups interv=-1, interv=0, and interv=1
  return(coef(msm.sw))
}
# bootstrap
results <- boot(data=nhefs, statistic=stabwei, R=1000)

# generating confidence intervals
se <- sd(results$t[,2])
mean <- mean(results$t[,2])
ll <- mean - qnorm(0.975)*se
ul <- mean + qnorm(0.975)*se

bootstrap <- data.frame( mean, se, ll, ul)
bootstrap

###########
## Q2 ##

nhefs$cens <- ifelse(is.na(nhefs$wt82), 1, 0)

# estimation of denominator of treatment weights
denom.fit2 <- glm(qsmk ~ sex + race + age + smokeintensity , 
                 family = binomial(), data = nhefs)
summary(denom.fit2)

pd2.qsmk <- predict(denom.fit2, type = "response")

# estimation of numerator of treatment weights
numer.fit2 <- glm(qsmk~1, family = binomial(), data = nhefs)
summary(numer.fit2)
pn2.qsmk <- predict(numer.fit2, type = "response")


# estimation of denominator of censoring weights
denom.cens2 <- glm(cens ~ qsmk + sex + race + age + asthma , 
                  family = binomial(), data = nhefs)
summary(denom.cens2)

pd2.cens <- 1-predict(denom.cens2, type = "response")

# estimation of numerator of censoring weights
numer.cens2 <- glm(cens~qsmk, family = binomial(), data = nhefs)
summary(numer.cens2)
pn2.cens <- 1-predict(numer.cens2, type = "response")

nhefs$sw.a2 <- ifelse(nhefs$qsmk == 0, ((1-pn2.qsmk)/(1-pd2.qsmk)),
                     (pn2.qsmk/pd2.qsmk))
nhefs$sw.c2 <- pn2.cens/pd2.cens
nhefs$sw2 <- nhefs$sw.c2*nhefs$sw.a2

##
msm.sw2 <- geeglm(wt82_71~qsmk, data=nhefs, 
                 weights=sw2, id=seqn, corstr="independence")
summary(msm.sw2)

beta <- 3.144
SE <- coef(summary(msm.sw2))[,2]
lcl <- beta-qnorm(0.975)*SE 
ucl <- beta+qnorm(0.975)*SE
a<-cbind(beta, lcl, ucl)
format(a, nsmall = 4)

########
## Q3 ####


# create a dataset with 3 copies of each subject
nhefs$interv <- -1 # 1st copy: equal to original one

interv0 <- nhefs # 2nd copy: treatment set to 0, outcome to missing
interv0$interv <- 0
interv0$qsmk <- 0
interv0$wt82_71 <- NA

interv1 <- nhefs # 3rd copy: treatment set to 1, outcome to missing
interv1$interv <- 1
interv1$qsmk <- 1
interv1$wt82_71 <- NA

onesample <- rbind(nhefs, interv0, interv1) # combining datasets

# linear model to estimate mean outcome conditional on treatment and confounders
# parameters are estimated using original observations only (nhefs)
# parameter estimates are used to predict mean outcome for observations with 
# treatment set to 0 (interv=0) and to 1 (interv=1)

std <- geeglm(wt82_71~qsmk + sex + race + age + smokeintensity, data=onesample, 
                  weights=sw.c2, id=seqn, corstr="independence")
summary(std)   
onesample$predicted_meanY <- predict(std, onesample)

# estimate mean outcome in each of the groups interv=0, and interv=1
# this mean outcome is a weighted average of the mean outcomes in each combination 
# of values of treatment and confounders, that is, the standardized outcome
mean(onesample[which(onesample$interv==-1),]$predicted_meanY)
mean(onesample[which(onesample$interv==0),]$predicted_meanY)
mean(onesample[which(onesample$interv==1),]$predicted_meanY)

a<- mean(onesample[which(onesample$interv==1),]$predicted_meanY)-mean(onesample[which(onesample$interv==0),]$predicted_meanY)
format(a, nsmall = 4)
#########
library("boot")
standardization <- function(data, indices) {
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
  fit <- geeglm(wt82_71~qsmk + sex + race + age + smokeintensity, data=d.onesample, 
         weights=sw.c2, id=seqn, corstr="independence")
  
  d.onesample$predicted_meanY <- predict(fit, d.onesample)
  
  # estimate mean outcome in each of the groups interv=-1, interv=0, and interv=1
  return(c(mean(d.onesample$predicted_meanY[d.onesample$interv==-1]),
           mean(d.onesample$predicted_meanY[d.onesample$interv==0]),
           mean(d.onesample$predicted_meanY[d.onesample$interv==1]),
           mean(d.onesample$predicted_meanY[d.onesample$interv==1])-
             mean(d.onesample$predicted_meanY[d.onesample$interv==0])))
}
# bootstrap
results <- boot(data=nhefs, statistic=standardization, R=1000)

# generating confidence intervals
se <- c(sd(results$t[,1]), sd(results$t[,2]), 
        sd(results$t[,3]), sd(results$t[,4]))
mean <- c(mean(results$t[,1]), mean(results$t[,2]), 
          mean(results$t[,3]), mean(results$t[,4]))
ll <- mean - qnorm(0.975)*se
ul <- mean + qnorm(0.975)*se

bootstrap <- data.frame(cbind(c("Observed", "No Treatment", "Treatment", 
                                "Treatment - No Treatment"), mean, se, ll, ul))
bootstrap
