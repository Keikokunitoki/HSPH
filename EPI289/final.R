library(dplyr)
library(readxl)
nhefs <- read_excel("C:/Users/keiko/Dropbox/2020/spring/EPI289/nhefs.xlsx")



nhefs$cens <- ifelse(is.na(nhefs$sbp), 1, 0)

table(nhefs$cens)
prop.table(table(nhefs$cens))

################################################
# Adjusting for confounding and selection bias #
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
