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

#Q4
salary$rank2 = 0
salary$rank3 = 0
salary$rank2[which(salary$rank == 2)] = 1
salary$rank3[which(salary$rank == 3)] = 1

fit2 <- lm(sal~ sex+rank2+rank3+years,data=salary)
summary(fit2)
