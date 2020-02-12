library("readxl")
data <- read_excel("C:/Users/keiko/Dropbox/2020/spring/SBS263/As1.xlsx")

fit1 <- lm(Health1~Income1, data=data)
summary(fit1)
confint.default(fit1)

fit2 <- lm(Health2~Income2, data=data)
summary(fit2)
confint.default(fit2)

fit3 <- lm(Health3~Income3, data=data)
summary(fit3)
confint.default(fit3)

fit4 <- lm(Health4~Income4, data=data)
summary(fit4)
confint.default(fit4)
