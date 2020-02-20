
grip <- read.table("C:/Users/keiko/Dropbox/2020/spring/BST226/compgrip.txt")
colnames(grip) = c("id", "trt", "y0", "y1", "y2", "y3")

grip_1 <- subset(grip, trt == 1)


# -- Calculate the desired statistics
tab_1 <- t(rbind(apply(grip_1, 2, length),
                 apply(grip_1, 2, mean),
                 apply(grip_1, 2, sd),
                 apply(grip_1, 2, var)))
tab_1

# -- Name the columns of the table
colnames(tab_1) <- c("N", "mean", "std_dev", "variance")


# -- Print the table
print(tab_1)


# -- Covariance matrix
cov(grip_1[,3:6])


# -- Correlation matrix
cor(grip_1[,3:6])


# -- Put data into "long" format
library(tidyr)
longgrip <- grip_1 %>% gather(week, y, y0:y3)


# -- Pull the numbers out of the week variables
longgrip$week <- as.numeric(substr(longgrip$week, 2,2))


# -- Make variable 'id' into a factor
longgrip$id <- factor(longgrip$id)


# -- Plot Spaghetti
library(ggplot2)
longgrip$id <-as.numeric(longgrip$id)
longgrip_8 <- longgrip %>% filter(id<=8)
longgrip$id <-as.factor(longgrip$id)
longgrip_8 %>% ggplot(aes(x = week, y = y, group = id)) +
  geom_line(aes(color = id), show.legend = TRUE) +
  xlab("Time (weeks)") + 
  ylab("Response")


# -- Calculate mean response and plot
longgrip %>%
  group_by(week) %>%
  summarize(mean.outcome = mean(y)) %>%
  ungroup() %>%
  ggplot(aes(week, mean.outcome)) +
  geom_line() +
  geom_point(size=5) +
  xlab("Time (weeks)") +
  ylab("Mean Response")


# -- Change weeks into factor w/ correct reference category
longgrip$week = relevel(factor(longgrip$week), ref="0")


# -- Run repeated measures model w/ unstructured correlation matrix
library(nlme)
model = gls(y ~ week, data=longgrip, corr=corSymm(form = ~ 1 | id), method = "REML",
            weights = varIdent(form = ~ 1 | week))


# -- Print model summary
summary(model)


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


getVarCov.fix(model, individual = 2) 


# -- F-stat
anova(model)


# -- Chi-Square stat - will be covered in Lab 2
Sigma <- vcov(model)
chi2_stat <- t(coef(model)[-1])%*%solve(Sigma[-1,-1])%*%(coef(model)[-1])
pchisq(chi2_stat, df = 3, lower.tail = FALSE)

