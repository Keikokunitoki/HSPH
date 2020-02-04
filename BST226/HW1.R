salary <- read.table("C:/Users/keiko/Dropbox/2020/spring/BST226/salary.txt", row.names=NULL)
View(salary)

library(dplyr)

salary <- salary %>%
  rename(
     ID=V1,
    sex=V2,
    years=V3,
    rank=V4,
    sal=V5
  )

#Q1
salary$sal <- as.numeric(salary$sal)
salary$sal
tapply(salary$sal, salary$sex, mean)

#Q2
fit <- lm(sal ~ sex, data=salary)
summary(fit)

#Q5
salary$rank2 = 0
salary$rank3 = 0
salary$rank2[which(salary$rank == 2)] = 1
salary$rank3[which(salary$rank == 3)] = 1

fit2 <- lm(sal~ sex+rank2+rank3+years,data=salary)
summary(fit2)


#Q8
salary$years_2 <- salary$years^2
fit3 <- lm(sal~ sex+rank2+rank3+years+years_2,data=salary)
summary(fit3)

#Q9
#sal_1 <- salary %>% filter(rank==1)
#fit4 <- lm(sal~ sex+years,data=sal_1)
#summary(fit4)
fit4 <- lm(sal~ sex*years+rank2+rank3,data = salary)
summary(fit4)
