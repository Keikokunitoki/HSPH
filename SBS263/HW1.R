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

plot(x=data$Income1,y=data$Health1,xlab="Income1",ylab="Health1",main="data1",color="blue")
abline(fit1, color="blue")

plot(x=data$Income2,y=data$Health2,xlab="Income2",ylab="Health2",main="data2",color="blue")
abline(fit2, color="blue")

plot(x=data$Income3,y=data$Health3,xlab="Income3",ylab="Health3",main="data3",color="blue")
abline(fit3, color="blue")

plot(x=data$Income4,y=data$Health4,xlab="Income4",ylab="Health4",main="data4",color="blue")
abline(fit4, color="blue")
