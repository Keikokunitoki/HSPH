library(dplyr)
library(readxl)
data <- read_excel("C:/Users/keiko/Dropbox/2020/spring/EPI289/nhefs.xlsx")

sdata <- data[order(data$seqn),]
sdata <- sdata %>% select(seqn,age,sex)
sdata<-sdata[1:10,]
sdata

summary(data$sbp)
se <- function(x) sqrt(var(x)/length(x))
se(data$sbp)
data$sbp
mean(data$sbp,na.rm = TRUE)
se(data$sbp,na.rm = TRUE)

library(psych)

describe(data$sbp,         type=2)  


data<- data %>% mutate(cutquar=cut(wt71, breaks=c(59.65, 69.40, 79.95), labels=c("1","2","3","4")))

wt71_c4 <- cut(data$wt71, breaks = c(36,59.65,69.40,79.95,170), labels= c("1","2","3","4"), right=FALSE, ordered_result=TRUE )

summary(wt71_c4)


data$quar
summary(data$wt71)

require(gmodels)
CrossTable(data$sex, data$race, expected = FALSE)


data <-  mutate(data, wt71_4 =ntile(wt71, 4))
data$wt71_4
summary(data$wt71_4)


fit <- lm(wt71 ~ smokeintensity, data=data)
summary(fit)

fit <- lm(wt71 ~ sex+age+race, data=data)
summary(fit)

predicted <- predict(fit) 
predicted

fit <- glm(wt71 ~ sex+age+race, data=data)
summary(fit)


fit <- glm(asthma ~ sex+age+race+active, data=data, family=binomial)
summary(fit)
predict(fit, data, type="response")



library(ggplot2)
data %>% ggplot() + geom_point(aes(x=data$active, y=data$sbp), alpha=0.2)+labs(x="physical activity",y="systoric blood pressure")
