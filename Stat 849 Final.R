Wine <- read.csv(file = 'winequality.csv')
sum.wine = summary(Wine)

Wine <- cbind(scale(Wine[1:11]), Wine[12])

# Baseline Model
library(car)
lmod1 <- lm(quality ~., data=Wine)
summary(lmod1)

# Influential Points
plot(lmod1, which = c(1,2,4,5), cook.levels = 1)

# Leverages
lmod1.hats = hatvalues(lmod1)
plot(lmod1.hats, type="h", ylab="Leverage")
text(lmod1.hats, cex=1)

# Outliers
library(MASS)
plot(studres(lmod1), type="h", ylab="Studentized residuals")
text(studres(lmod1), cex=1)
abline(h=c(-3,0,3), lty=2)

# Remove unusual observations
Wine2 <- Wine[-c(2781, 4745, 3307),]
lmod2 <- lm(quality ~., data=Wine2)
summary(lmod2)
plot(lmod2, which = c(1,2,4,5), cook.levels = 1)

# Multicollinearity
library(corrplot)
corrplot(cor(Wine2))

vif2 <- vif(lmod2)
print(xtable(as.table(vif2), type = "latex"), file = "vif2.tex")

lmod3 <- lm(quality ~.-density-total.sulfur.dioxide, data=Wine2)
vif3 <- vif(lmod3)

# Best Subset Selection
library(leaps)
lmod4 <- regsubsets(quality ~.-citric.acid-density-total.sulfur.dioxide, data=Wine2, nvmax=8, method="exhaustive")
rs <- summary(lmod4)
par(mfrow=c(1,3))
plot(rs$adjr2, xlab="No. of Predictors", ylab="Adjusted R-square")
plot(rs$cp, xlab="No. of Predictors", ylab="Cp Statistic")
plot(rs$bic, xlab="No. of Predictors", ylab="BIC")
which.max(rs$adjr2) 
which.min(rs$cp)
which.min(rs$bic)
rs$which

lmod5 <- lm(quality ~.-density-total.sulfur.dioxide-citric.acid, data=Wine2)
summary(lmod5)
lmod6 <- lm(quality ~.-density-total.sulfur.dioxide-citric.acid-pH-chlorides, data=Wine2)
summary(lmod6)

# LASSO
library(glmnet)
X <- model.matrix(quality ~.-density-total.sulfur.dioxide-citric.acid, data=Wine2)[, -1]
Y <- Wine2$quality
cv.out <- cv.glmnet(X, Y, alpha=1) 
par(mfrow=c(1,1))
plot(cv.out)
lambda_opt <- cv.out$lambda.min
predict(cv.out, s = lambda_opt, type = "coefficients")[1:9, ]
lambda_opt

# Output LaTeX tables
library(texreg)
texreg(list(lmod1, lmod2, lmod5, lmod6))

knitr::stitch('Stat 849 Final.R')