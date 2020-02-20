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
nhefs.nmv$sw <- ifelse(nhefs.nmv$qsmk==1, nhefs$predict_nnhefs$predict_d,(1-nhefs$predict_n)/(1-nhefs$predict_d))
summary(nhefs.nmv$w)
summary(nhefs.nmv$sw)

# marginal structural model with conservative 95% CI
library("geepack")
msm.valid <- geeglm(wt82_71 ~ qsmk, data=nhefs.nmv, weights=w, id=seqn,
                    corstr="independence")
summary(msm.valid)

beta <- coef(msm.valid)
SE <- coef(summary(msm.valid))[,2]
lcl <- beta-qnorm(0.975)*SE   #because of pseudo population, we have to use robust SE, bit wider than default
ucl <- beta+qnorm(0.975)*SE
cbind(beta, lcl, ucl)

#in boot strap, we have to whole process including calcurating IP value


#############
### part2 ###
#############
table(nhefs.nmv$age[nhefs.nmv$race == 0 & nhefs.nmv$sex == 0], 
      nhefs.nmv$qsmk[nhefs.nmv$race == 0 & nhefs.nmv$sex == 0])
q2.nosmic <-data.frame(table(
  subset(nhefs,qsmk==0)$age,
  subset(nhefs,qsmk==0)$sex,
  subset(nhefs,qsmk==0)$race))
colnames
table(nhefs.nmv$age[nhefs.nmv$race == 0 & nhefs.nmv$sex == 1], 
      nhefs.nmv$qsmk[nhefs.nmv$race == 0 & nhefs.nmv$sex == 1])

#############
### part3 ###
#############

#### msm ###
fit <- glm(qsmk ~ sex + race + age + I(age*age), family=binomial(),
           data=nhefs)
summary(fit)
nhefs$p.qsmk <- predict(fit, nhefs, type="response")

nhefs$w <- ifelse(nhefs$qsmk==1, 1/nhefs$p.qsmk, 1/(1-nhefs$p.qsmk))
#nhefs.full$sw <- ifelse(nhefs$==1, )
summary(nhefs$w)

#weighted.mean(subset(nhefs.full,qsmk==1)$death, subset(nhefs.full, qsmk==1)$w)
#weighted.mean(subset(nhefs.full,qsmk==0)$death, subset(nhefs.full, qsmk==0)$w)

# marginal structural model with conservative 95% CI
library("geepack")
msm.death <- geeglm(death ~ qsmk, data=nhefs, weights=w, id=seqn,
                    corstr="independence", family=binomial)
summary(msm.death)

beta <- coef(msm.death)
SE <- coef(summary(msm.death))[,2]
lcl <- beta-qnorm(0.975)*SE 
ucl <- beta+qnorm(0.975)*SE
cbind(beta, lcl, ucl)

#95% risk difference
msm.death <- geeglm(death ~ qsmk, data=nhefs, weights=w, id=seqn,
                    corstr="independence", family=binomial(link=independence))
