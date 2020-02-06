
library("readxl")
nhefs <- read_excel("C:/Users/keiko/Dropbox/2020/spring/EPI289/nhefs.xlsx")
nhefs.nd <- nhefs[which(!is.na(nhefs$death)),]




# PS: Parametric estimation with many covariates
fit.para1 <- glm(qsmk ~ sex + race + age + I(age*age) + as.factor(education) 
                 + as.factor(active), 
                 data=nhefs.nd, family=binomial())
summary(fit.para1)

nhefs.nd$p.qsmk1 <- predict(fit.para1, type="response")
head(cbind(nhefs.nd$seqn, nhefs.nd$sex, nhefs.nd$age, nhefs.nd$p.qsmk1))
summary(nhefs.nd$p.qsmk1)

## regression on continuous PS
fit.ps.cont <- glm(death~qsmk+p.qsmk1+I(p.qsmk1*p.qsmk1), data=nhefs.nd)
summary(fit.ps.cont)

exp(fit.ps.cont$coefficients)
exp(confint.default(fit.ps.cont))



############
## Part 2 ##

nhefs.nd$age50 <- ifelse(nhefs.nd$age>50, 1, 0)

prop.table(table(nhefs.nd$sex, nhefs.nd$age50, nhefs.nd$race))

nrow(nhefs.nd[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==0])
nrow(nhefs.nd$qsmk[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==0])

summary(nhefs.nd$death[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==0 & nhefs.nd$qsmk==0])
summary(nhefs.nd$death[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==0 & nhefs.nd$qsmk==0])
summary(nhefs.nd$death[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==0 & nhefs.nd$qsmk==1])

# summary(nhefs.nd$qsmk[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==1])
summary(nhefs.nd$death[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==1 & nhefs.nd$qsmk==0])
summary(nhefs.nd$death[nhefs.nd$age50==0 & nhefs.nd$sex==0 & nhefs.nd$race==1 & nhefs.nd$qsmk==1])
