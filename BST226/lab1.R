#Change your working directory to where tlc.txt is on your laptop/computer
setwd("C:/Users/Daniel Li/Desktop/Spring 2020/BST 226 - Longitudinal TA/Labs/Lab 1")

#Install packages if needed
install.packages("ggplot2")
install.packages("nlme")
library(ggplot2)
library(nlme)

#Slide 7 - read in data, add column names
lead = read.table("tlc.txt")
colnames(lead) = c("id", "trmt", "y1", "y2", "y3", "y4")
lead[1:5,]

#Slide 8-9 - subset to treatment "A" only
lead2 = lead[which(lead$trmt == "A"),]

#Slide 10 - view data
lead2[1:5,]

#Slide 11 - means over time
summary(lead2)

#Slide 12 - covariance matrix
cov(lead2[,3:6])
cor(lead2[,3:6])

#Slide 16 - wide form to long form
id = as.factor(rep(lead2$id, each=4))
trmt = rep("A", 200)
y = as.vector(t(as.matrix(lead2[,3:6])))
time = rep(c(0, 1, 4, 6), 50)
t = as.factor(rep(c(1, 2, 3, 4), 50))

lead3 = data.frame(id, trmt, y, time, t)
lead3[1:5,]

#Slide 19 - spaghetti plot
p <- ggplot(data = lead3, aes(x = time, y = y, group = id, colour=id))
p + geom_line() + labs(x = "Time in Weeks", y = "Blood Lead Level (ug/dL)", 
       title = "Spaghetti Plot of Observed Trajectories")

#Slide 20 - mean response versus time
plot(c(0, 1, 4, 6), colMeans(lead2[,3:6]), type="b",
     main="Mean Blood Lead Level Versus Time",
     xlab="Time in Weeks", ylab="Blood Lead Level (ug/dL)")

#Slide 23 - time categorical, "t=6" as reference
#Note: default is "t=0" as reference
#Point estimates and p-values closely match
lead3$time = as.factor(lead3$time)
lead3$time = relevel(lead3$time, ref="6")
model = gls(y ~ time, data=lead3, corr=corSymm(form = ~ 1 | id),
            weights = varIdent(form = ~ 1 | t))
summary(model)

#Slide 29 - time categorical, "t=0" as reference
#Point estimates and p-values closely match
lead3$time = relevel(lead3$time, ref="0")
model = gls(y ~ time, data=lead3, corr=corSymm(form = ~ 1 | id),
            weights = varIdent(form = ~ 1 | t))
summary(model)

#Slide 31 - time continuous
#Point estimates and test statistics closely match
#P-values slightly different, maybe due to rounding of test statistics, differences in df, or other mechanics
#SAS presents t=-3.20, R presents t=-3.2044
lead3$time = rep(c(0, 1, 4, 6), 50)
model = gls(y ~ time, data=lead3, corr=corSymm(form = ~ 1 | id),
            weights = varIdent(form = ~ 1 | t))
summary(model)

