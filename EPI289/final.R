library(dplyr)
library(readxl)
nhefs <- read_excel("C:/Users/keiko/Dropbox/2020/spring/EPI289/nhefs.xlsx")

nhefs$cens <- ifelse(is.na(nhefs$sbp), 1, 0)

table(nhefs$cens)
prop.table(table(nhefs$cens))

################################################
#   Part 2 unstabilized   #
################################################

# estimation of denominator of treatment weights
denom.fit <- glm(qsmk ~ sex + race + age + as.factor(education) + smokeintensity + 
                   as.factor(exercise) + as.factor(active) + wt71, 
                 family = binomial(), data = nhefs)
summary(denom.fit)

nhefs$pd.qsmk <- predict(denom.fit, type = "response")

# estimation of denominator of censoring weights
denom.cens <- glm(cens ~ qsmk + sex + race + age + as.factor(education) + 
                smokeintensity +  as.factor(exercise) + as.factor(active) + wt71, 
                  family = binomial(), data = nhefs)
summary(denom.cens)

nhefs$pd.cens <- 1-predict(denom.cens, type = "response")

#weighhts
nhefs$w.a <- ifelse(nhefs$qsmk==1, 1/nhefs$pd.qsmk, 1/(1-nhefs$pd.qsmk))
nhefs$w.c <- 1/pd.cens
nhefs$w <- nhefs$w.c*nhefs$w.a

summary(nhefs$w.a)
sd(nhefs$w.a)
summary(nhefs$w.c)
sd(nhefs$w.c)
summary(nhefs$w)
sd(nhefs$w)

#
library(geepack)
msm.w <- geeglm(sbp~qsmk, data=nhefs, 
                 weights=w, id=seqn, corstr="independence")
summary(msm.w)

beta <- coef(msm.w)
SE <- coef(summary(msm.w))[,2]
lcl <- beta-qnorm(0.975)*SE 
ucl <- beta+qnorm(0.975)*SE
cbind(beta, lcl, ucl)



################################################
#   Part 3 stabilized   #
################################################

# estimation of denominator of treatment weights
denom.fit <- glm(qsmk ~ sex + race + age + as.factor(education) + smokeintensity + 
                   as.factor(exercise) + as.factor(active) + wt71, 
                 family = binomial(), data = nhefs)
summary(denom.fit)
nhefs$pd.qsmk <- predict(denom.fit, type = "response")

# estimation of numerator of treatment weights
numer.fit <- glm(qsmk~1, family = binomial(), data = nhefs)
summary(numer.fit)
pn.qsmk <- predict(numer.fit, type = "response")

# estimation of denominator of censoring weights
denom.cens <- glm(cens ~ qsmk + sex + race + age + as.factor(education) + 
                    smokeintensity +  as.factor(exercise) + as.factor(active) + wt71, 
                  family = binomial(), data = nhefs)
summary(denom.cens)
nhefs$pd.cens <- 1-predict(denom.cens, type = "response")

# estimation of numerator of censoring weights
numer.cens <- glm(cens~qsmk, family = binomial(), data = nhefs)
summary(numer.cens)
pn.cens <- 1-predict(numer.cens, type = "response")

#weighhts
nhefs$sw.a <- ifelse(nhefs$qsmk == 0, ((1-pn.qsmk)/(1-pd.qsmk)),
                     (pn.qsmk/pd.qsmk))
nhefs$sw.c <- pn.cens/pd.cens
nhefs$sw <- nhefs$sw.c*nhefs$sw.a

summary(nhefs$sw.a)
sd(nhefs$sw.a)
summary(nhefs$sw.c)
sd(nhefs$sw.c)
summary(nhefs$sw)
sd(nhefs$sw)

#
library(geepack)
msm.sw <- geeglm(sbp~qsmk, data=nhefs, 
                weights=sw, id=seqn, corstr="independence")
summary(msm.sw)

beta <- coef(msm.sw)
SE <- coef(summary(msm.sw))[,2]
lcl <- beta-qnorm(0.975)*SE 
ucl <- beta+qnorm(0.975)*SE
cbind(beta, lcl, ucl)


########################
# part 4 G-estimation #
#######################

####################################################
# G-estimation + IPW for selection bias adjustment #
####################################################

nhefs$c <- ifelse(is.na(nhefs$sbp), 1, 0)

# estimation of denominator of censoring weights
cw.denom <- glm(c==0 ~ qsmk+ sex + race + age + as.factor(education) + 
                  smokeintensity +  as.factor(exercise) + as.factor(active) + wt71, 
                family=binomial, data=nhefs)

nhefs.c <- nhefs[which(!is.na(nhefs$sbp)),]
nhefs.c$pd.c <- predict(cw.denom, nhefs.c, type="response")

nhefs.c$wc <- 1/(nhefs.c$pd.c)
summary(nhefs.c$wc)


#######################################
# G-estimation: Closed-form estimator #
#######################################

logit.est <- glm(qsmk ~ sex + race + age + as.factor(education) + 
                   smokeintensity +  as.factor(exercise) + as.factor(active) + wt71, 
                 family=binomial(), data=nhefs.c, weight=wc)
summary(logit.est)
nhefs.c$pqsmk <- predict(logit.est, nhefs.c, type = "response")
summary(nhefs.c$pqsmk)

# solve sum(w_c * H(psi) * (qsmk - E[qsmk | L]))  = 0
# for a single psi and H(psi) = wt82_71 - psi * qsmk
# this can be solved as psi = sum( w_c * wt82_71 * (qsmk - pqsmk)) / sum(w_c * qsmk * (qsmk - pqsmk))

with(nhefs.c, sum(wc*sbp*(qsmk - pqsmk)) / sum(wc*qsmk*(qsmk - pqsmk)))  # =psi

# finding the approximate 95% CI
#install.packages("geepack")
library("geepack")
grid <- seq(from = 0.5,to = 3.5, by = 0.1)
j = 0
Hpsi.coefs <- cbind(rep(NA,length(grid)), rep(NA, length(grid)))
colnames(Hpsi.coefs) <- c("Estimate", "p-value")

for (i in grid){
  psi = i
  j = j+1
  nhefs.c$Hpsi <- nhefs.c$sbp - psi * nhefs.c$qsmk 
  
  gest.fit <- geeglm(qsmk ~ sex + race + age + as.factor(education) + 
                       smokeintensity +  as.factor(exercise) + as.factor(active) + wt71 + Hpsi, 
                     family=binomial, data=nhefs.c,
                     weights=wc, id=seqn, corstr="independence")
  Hpsi.coefs[j,1] <- summary(gest.fit)$coefficients["Hpsi", "Estimate"]
  Hpsi.coefs[j,2] <- summary(gest.fit)$coefficients["Hpsi", "Pr(>|W|)"]
}
Hpsi.coefs




###############################
# Part 5 outcome regression #
##############################

model <- glm(sbp ~ qsmk +sex + race + age + as.factor(education) + 
               smokeintensity +  as.factor(exercise) + as.factor(active) + wt71 +cens, data=nhefs)
summary(model)
confint(model, 'qsmk', level=0.95)

############################
## Part 6 Propensity Score ##
#############################

# PS: Parametric estimation with many covariates
fit.para <- glm(qsmk ~ sex + race + age + as.factor(education) + 
                  smokeintensity +  as.factor(exercise) + as.factor(active) + wt71 ,
                 data=nhefs, family=binomial())
summary(fit.para)


nhefs$p.qsmk <- predict(fit.para, type="response")
head(cbind(nhefs$seqn, nhefs$sex, nhefs$age, nhefs$p.qsmk))

summary(nhefs$p.qsmk)

#install.packages("psych") # install package if required
library("psych")

## regression on continuous PS
fit.ps.cont <- glm(sbp~qsmk+p.qsmk+I(p.qsmk*p.qsmk),weights=w.c, data=nhefs)
summary(fit.ps.cont)
confint(fit.ps.cont, 'qsmk', level=0.95)


####################
# Medicaid   #
#################
ohie <- read.csv("C:/Users/keiko/Dropbox/2020/spring/EPI289/ohie_2020.csv")
table(ohie$lottery, ohie$medicaid)
table(ohie$lottery)

#install.packages ("sem") # install package if required
library(sem) 

model1 <- tsls(bmi ~ medicaid, ~ lottery, data = ohie)
summary(model1)
confint(model1) 
