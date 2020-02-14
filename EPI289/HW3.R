library(dplyr)
library(readxl)
nhefs <- read_excel("C:/Users/keiko/Dropbox/2020/spring/EPI289/nhefs.xlsx")
nhefs.nmv <- nhefs[which(!is.na(nhefs$wt82)),] 


#### msm ###
fit <- glm(qsmk ~ sex + race + age + I(age*age), family=binomial(),
           data=nhefs.nmv)
summary(fit)
nhefs.nmv$p.qsmk <- predict(fit, nhefs.nmv, type="response")

nhefs.nmv$w <- ifelse(nhefs.nmv$qsmk==1, 1/nhefs.nmv$p.qsmk, 1/(1-nhefs.nmv$p.qsmk))
summary(nhefs.nmv$w)

# marginal structural model with conservative 95% CI
library("geepack")
msm.valid <- geeglm(wt82_71 ~ qsmk, data=nhefs.nmv, weights=w, id=seqn,
                    corstr="independence")
summary(msm.valid)

beta <- coef(msm.valid)
SE <- coef(summary(msm.valid))[,2]
lcl <- beta-qnorm(0.975)*SE 
ucl <- beta+qnorm(0.975)*SE
cbind(beta, lcl, ucl)


#############
### part2 ###
#############
table(nhefs.nmv$age[nhefs.nmv$race == 0 & nhefs.nmv$sex == 0], 
      nhefs.nmv$qsmk[nhefs.nmv$race == 0 & nhefs.nmv$sex == 0])
table(nhefs.nmv$age[nhefs.nmv$race == 0 & nhefs.nmv$sex == 1], 
      nhefs.nmv$qsmk[nhefs.nmv$race == 0 & nhefs.nmv$sex == 1])

#############
### part3 ###
#############

#### msm ###
fit <- glm(qsmk ~ sex + race + age + I(age*age), family=binomial(),
           data=nhefs.nmv)
summary(fit)
nhefs.nmv$p.qsmk <- predict(fit, nhefs.nmv, type="response")

nhefs.nmv$w <- ifelse(nhefs.nmv$qsmk==1, 1/nhefs.nmv$p.qsmk, 1/(1-nhefs.nmv$p.qsmk))
summary(nhefs.nmv$w)

# marginal structural model with conservative 95% CI
library("geepack")
msm.death <- geeglm(death ~ qsmk, data=nhefs.nmv, weights=w, id=seqn,
                    corstr="independence", family=binomial)
summary(msm.death)

beta <- coef(msm.death)
SE <- coef(summary(msm.death))[,2]
lcl <- beta-qnorm(0.975)*SE 
ucl <- beta+qnorm(0.975)*SE
cbind(beta, lcl, ucl)
