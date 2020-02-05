hw1 <- read.csv("C:/Users/keiko/Dropbox/2020/spring/EPI289/hmwk1.csv")

model <- glm(death~smk, data=hw1,family=binomial)
summary(model)
#add link logit
model <- glm(death~smk, data=hw1,family=binomial(link="logit"))
summary(model)

exp(model$coefficients)
exp(confint.default(model))


model <- glm(death~smk *drink, data=hw1,family=binomial)
summary(model)
#wecan write smk + drink +smk:drink
#* includes all possible combination of the ccovariates

model <- glm(death~smk + overwt, data=hw1,family=binomial)
summary(model)

############
library(readxl)
data <- read_excel("C:/Users/keiko/Dropbox/2020/spring/EPI289/nhefs.xlsx")

data$age2 <- data$age^2

model <- glm(death~qsmk + sex + race +age +age2, data=data,family=binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))
#or I can write +I(age*age) for quadratic term
#"I" tells R we are using new terms 

data$smokeyrs2 <- data$smokeyrs^2

model <- glm(death~qsmk + sex + race +age +age2 + smokeyrs +smokeyrs2
            , data=data,family=binomial)
summary(model)
exp(cbind(OR = coef(model), confint(model)))

##########
#Generate a temporary dataset that excludes observations with missing values for weight gain and 
#create the following categories of age (25-40, 41-60, and >60).
library(dplyr)
data1 <- data %>% filter(wt82_71!="NA")

data1 <- data1 %>% mutate(age_cat="NA") 
for (a in data1$age){
  if (a>=25 & a<=40) {
    data1$age_cat[data1$age==a] <- 1
  }
  else if (a>=41 & a<=60) {
    data1$age_cat[data1$age==a] <- 2
  }
  else if (a>61) {
    data1$age_cat[data1$age==a] <- 3
  }
  else {
    data1$age_cat[data1$age==a] <- "NA"
  }
}

data1$age_cat


#Use PROC RANK in SAS or cut in R to generate a categorical variables with 5, 10, 20 and 49 categories of age.
data1 <-  mutate(data1, age_5 =ntile(age, 5))
data1 <-  mutate(data1, age_10 =ntile(age, 10))
data1 <-  mutate(data1, age_20 =ntile(age, 20))
data1 <-  mutate(data1, age_49 =ntile(age, 49))

#Use GPLOT in SAS or plot in R to graphically explore the relation between age and weight gain.
plot(data1$age, data1$wt82_71)
plot(data1$age_cat, data1$wt82_71)
plot(data1$age_5, data1$wt82_71)

#Fit a linear regression model of the form wt82_71= α0 + α1Age with age as a continuous variable and 
#plot the values predicted by the model against the observed values.
fit <- glm(wt82_71 ~ age, data=data1)
summary(fit)
data1$predicted <- predict(fit) 

plot(data1$age,data1$wt82_71,xlim=c(25,75), ylim=c(-45,45))
par(new=T)
plot(data1$age,data1$predicted,col="blue",xlim=c(25,75), ylim=c(-45,45))

#x=data1$age
#y=data1$wt82_71
#plot(x,y,col = "blue",main = "wtchange age Regression",
#     abline(lm(y~x)),cex = 1,pch = 20,xlab = "age",ylab = "weight change")



#Similarly, fit a linear regression model of the form wt82_71= α0 + α1Age + α2Age2 and 
#plot the values predicted by the model against the observed values.
fit <- glm(wt82_71 ~ age+ age2, data=data1)
summary(fit)
data1$predicted <- predict(fit) 
plot(data1$age,data1$wt82_71,xlim=c(25,75), ylim=c(-45,45))
par(new=T)
plot(data1$age,data1$predicted,col="blue",xlim=c(25,75), ylim=c(-45,45))

#Fit a linear regression model including age in 3, 5, 10, 20 and 49 categories and 
#plot the values predicted by the model against the observed values.
fit <- glm(wt82_71 ~ age+age_cat+ age_5+age_10+age_20+age_49, data=data1)
summary(fit)
data1$predicted <- predict(fit) 
plot(data1$age,data1$wt82_71,xlim=c(25,75), ylim=c(-45,45))
par(new=T)
plot(data1$age,data1$predicted,col="blue",xlim=c(25,75), ylim=c(-45,45))
