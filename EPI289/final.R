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
