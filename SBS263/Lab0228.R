library(lme4)
library(nlme)
data <- read.csv ("C:/Users/keiko/Dropbox/2020/spring/SBS263/house.csv")

head(data)
data$house <- data$i..house

m0 <- lm(price~1, data=data)
mo_sum <- summary(lm(price~+1,data=data))

m0$coefficients
m0$residuals

m0 <- lmer(price ~ 1+(1|district),data=data)
summary(m0)

sum_m0 <- summary(m0)
sum_m0$varcor[1]
ranef(m0)
m0_group_residuals <- as.data.frame(ranef(m0))
m0_group_residuals$grpvar <- m0_group_residuals$term <- NA

summary(lm(price~1+factor(district),data=data))

fe <- summary(lm(price~1+factor(district),data))
fe$coefficient[34]

summary(lmer(price~size+(size|district),data=data))
