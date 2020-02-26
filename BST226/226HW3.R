
#These hints may further show that using SAS for this class may be easiest
#Working in R requires some additional data clean up and finessing

#Libraries and setup
setwd("C:/Users/Daniel Li/Desktop/Spring 2020/BST 226 - Longitudinal TA/Data")
library(nlme)

##############Part 1#############
#Tweaks to reading in data
#Need to specify that periods equal missing, otherwise they will be read in as characters
data = read.table("data.txt", na.string=".")

#Some categorical variables may be read in as continuous
data$var = as.factor(data$var)

#Summary statistics by group
#Can modify mean function to standard deviation (sd) or variance (var)
#Need to specify na.rm=TRUE so missing values will be ignored
tapply(X = df$y, INDEX = df$group, FUN = mean, na.rm=TRUE)

#Can also use tidyverse as in HW2 too
#Not necessary though, as long as output is correct

##############Part 2#############
#Plot of mean vs time
#Can use your calculated means from above and the normal plot function
plot(x=x, y=y, main="", xlab="", ylab="", col="")
lines(x=x, y=y, col="")
legend("bottomright", inset=0.02, 
       legend=c("Treatment 1", "Treatment 2"),
       col=c("", ""), lty=c(1,1), cex=0.8)

#Those interested in learning/using ggplot can do so
#Again as long as output is correct

##############Formatting Long Form#############
## reshape in R from wide to long
longdata <- reshape(data=data, idvar="id", 
                    varying=c("y1", "y2", "y3", "y4", "y5"), v.names="y", 
                    timevar = "time", times=1:5, direction="long")

#As above, make sure necessary variables (such as time) are factors
#Can check using summary() or class()

#Need to set treatment reference to 2
longdata$trt = relevel(longdata$trt, ref="2")

##############Part 3#############
#Can obtain fitted model below
#Output very similar to SAS output
model = gls(y ~ var1*var2, data=longdata, corr=corSymm(form = ~ 1 | id),
            weights = varIdent(form = ~ 1 | time), na.action = na.omit)
summary(model)

#F statistic
anova(model)

#Chi-squared statistic
Sigma <- vcov(model)
chi2_stat <- t(coef(model)[7:10])%*%solve(Sigma[7:10, 7:10])%*%(coef(model)[7:10])
pchisq(chi2_stat, df = 4, lower.tail = FALSE)

#Likelihood ratio test
#Use ML insead of REML (default)
model.full = gls(y ~ trt*time, data=longdata, corr=corSymm(form = ~ 1 | id),
                 weights = varIdent(form = ~ 1 | time), na.action = na.omit, method="ML")

model.null = gls(y ~ trt + time, data=longdata, corr=corSymm(form = ~ 1 | id),
                 weights = varIdent(form = ~ 1 | time), na.action = na.omit, method="ML")
anova(model.full, model.null)

##############Part 4#############
#Outcome correlation matrix is in the summary output

#VarCov similar to SAS output
#Do NOT change this function, just run it so we define a new function
getVarCov.fix <- function (obj, individual = 1, ...) {
  S <- corMatrix(obj$modelStruct$corStruct)[[individual]]
  if (!is.null(obj$modelStruct$varStruct)) {
    ## ind <- obj$groups == individual                                      # index from dataset
    dimensions.Ri <- sapply(corMatrix(obj$modelStruct$corStruct), nrow)     # dimentions from corMatrix()
    ind <- rep(seq_along(dimensions.Ri), dimensions.Ri) == individual       # index from dimensions
    vw <- 1/varWeights(obj$modelStruct$varStruct)[ind]
  }
  else vw <- rep(1, nrow(S))
  vars <- (obj$sigma * vw)^2
  result <- t(S * sqrt(vars)) * sqrt(vars)
  class(result) <- c("marginal", "VarCov")
  attr(result, "group.levels") <- names(obj$groups)
  result
}

#Can change input here, do not change individual input
getVarCov.fix(model, individual = 2) 

#Correlation matrix in summary output from part 3

##############Parts 5 and 6#############
#Refer to summary output from part 3