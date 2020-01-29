#Q1
library(dplyr)
library(readxl)
data <- read_excel("C:/Users/keiko/Dropbox/2020/spring/EPI289/nhefs.xlsx")

#Q2
sdata <- data[order(data$seqn),]
sdata <- sdata %>% select(seqn,age,sex)
sdata<-sdata[1:10,]
sdata

#Q3
summary(data$sbp)
se <- function(x) sqrt(var(x)/length(x))
se(data$sbp)
data$sbp
mean(data$sbp,na.rm = TRUE)
se(data$sbp,na.rm = TRUE)

library(psych)
describe(data$sbp, type=2)  

#q4
summary(data$wt71)

#Q5
data <- mutate(data, wt71_cw4 = case_when(wt71<59.65 ~ "1",
                                     wt71>=59.65 & wt71<69.4 ~ "2",
                                     wt71>=69.4 & wt71<79.95 ~ '3',
                                     wt71>=79.95 ~ '4'))
data$wt71_cw4 <- as.factor(data$wt71_cw4)
summary(data$wt71_cw4)


data<- data %>% mutate(cutquar=cut(wt71, breaks=c(59.65, 69.40, 79.95), labels=c("1","2","3","4")))

ApplyQuartiles <- function(x) {
  cut(x, breaks=c(quantile(data$wt71, probs = seq(0, 1, by = 0.25))), 
      labels=c("1","2","3","4"), include.lowest=TRUE)
}
data$Quartile <- sapply(data$wt71, ApplyQuartiles)
table(data$Quartile)


wt71_c4 <- cut(data$wt71, breaks = c(36,59.65,69.40,79.95,170), labels= c("1","2","3","4"), right=FALSE, ordered_result=TRUE )
summary(wt71_c4)


#5b
data <-  mutate(data, wt71_4 =ntile(wt71, 4))
data$wt71_4 <- as.factor(data$wt71_4)
summary(data$wt71_4)

#Q6
fit <- lm(wt71 ~ smokeintensity, data=data)
summary(fit)

#7
require(gmodels)
CrossTable(data$sex, data$race, expected = FALSE)

#8
fit <- lm(wt71 ~ sex+age+race, data=data)
summary(fit)


predicted <- predict(fit) 
predicted

#9
fit <- glm(wt71 ~ sex+age+race, data=data)
summary(fit)

#q10
fit <- glm(asthma ~ sex+age+race+active, data=data, family=binomial)
summary(fit)
predict(fit, data, type="response")


#Q11
library(ggplot2)
data %>% ggplot() + geom_point(aes(x=data$active, y=data$sbp), alpha=0.2)+labs(x="physical activity",y="systoric blood pressure")
