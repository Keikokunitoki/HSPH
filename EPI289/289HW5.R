library("readxl")
nhefs <- read_excel("C:/Users/keiko/Dropbox/2020/spring/EPI289/nhefs.xlsx")

summary(nhefs$price82)

# for simplicity, ignore subjects with missing outcome or missing instrument
nhefs.iv <- nhefs[which( !is.na(nhefs$price82)),]
nhefs.iv$highprice <- ifelse(nhefs.iv$price82>=1.5, 1, 0)

table(nhefs.iv$highprice, nhefs.iv$qsmk)
table(nhefs.iv$highprice)

t.test(wt82_71 ~ highprice, data=nhefs.iv)
chisq.test(nhefs.iv$qsmk , nhefs.iv$highprice,, correct=FALSE)

######################################################################
# Estimating the average causal effect using the standard IV estimator 
# via two-stage-least-squares regression
######################################################################

#install.packages ("sem") # install package if required
library(sem) 

model1 <- tsls(wt82_71 ~ qsmk, ~ highprice, data = nhefs.iv)
summary(model1)
confint(model1) 


######################################################################
# Estimating the average causal using the standard IV estimator
# with altnerative proposed instruments
######################################################################

summary(tsls(wt82_71 ~ qsmk, ~ ifelse(price82 >= 1.6, 1, 0), data = nhefs.iv))
summary(tsls(wt82_71 ~ qsmk, ~ ifelse(price82 >= 1.7, 1, 0), data = nhefs.iv))
summary(tsls(wt82_71 ~ qsmk, ~ ifelse(price82 >= 1.8, 1, 0), data = nhefs.iv))
summary(tsls(wt82_71 ~ qsmk, ~ ifelse(price82 >= 1.9, 1, 0), data = nhefs.iv))


######################################################################
# Estimating the average causal using the standard IV estimator
# Conditional on baseline covariates
######################################################################

model2 <- tsls(wt82_71 ~ qsmk + sex + race + age + smokeintensity + smokeyrs + 
                 as.factor(exercise) + as.factor(active) + wt71,
               ~ highprice + sex + race + age + smokeintensity + smokeyrs + as.factor(exercise) +
                 as.factor(active) + wt71, data = nhefs.iv)
summary(model2)

###################################
###for HW

####test censor

nhefs.iv$cens <- ifelse(is.na(nhefs.iv$wt82_71), 1, 0)

# estimation of denominator of censoring weights
denom.cens <- glm(cens ~ qsmk + sex + race + age , 
                   family = binomial(link='logit'), data = nhefs.iv)
summary(denom.cens)
pd.cens <- 1-predict(denom.cens, type = "response")

nhefs.iv$w.c <- 1/pd.cens


model3 <- tsls(wt82_71 ~ qsmk ,
               ~ highprice ,weights=w.c,  data = nhefs.iv)
summary(model3)  #stand error is not robust, to see correct CI, we have to do bootstrapping
confint(model3)

#in stabilised weighting, there are bias (see DAG)

###manual calculation
##we have to limit the population to those who complete the study
nhefs.iv2 <- nhefs.iv[which(nhefs.iv$cens!=1),]

YZ1<- weighted.mean(nhefs.iv2$wt82_71[nhefs.iv2$highprice ==1],nhefs.iv2$w.c[nhefs.iv2$highprice==1])
YZ0<- weighted.mean(nhefs.iv2$wt82_71[nhefs.iv2$highprice ==0],nhefs.iv2$w.c[nhefs.iv2$highprice==0])
AZ1<- weighted.mean(nhefs.iv2$qsmk[nhefs.iv2$highprice ==1],nhefs.iv2$w.c[nhefs.iv2$highprice==1])
AZ0<- weighted.mean(nhefs.iv2$qsmk[nhefs.iv2$highprice ==0],nhefs.iv2$w.c[nhefs.iv2$highprice==0])
meaneffect <- (YZ1-YZ0)/(AZ1-AZ0) 
meaneffect
