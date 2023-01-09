# Q1

lmod1 <- lm(conc ~ hour, data = solution)
summary(lmod1)
plot(lmod1)
plot(x = solution$hour, y = solution$conc,
     main = "Concentration vs Time", xlab = "Time", ylab = "Concentration")
abline(lmod1)

lmod2 <- lm(conc ~ factor(hour), data = solution)
anova(lmod1, lmod2)

require(MASS)
boxcox(lmod1, lambda=seq(-0.25,0.75,by=0.05),plotit=T)
lambda_opt <- 0.04
trans_conc <- (solution$conc^(lambda_opt)-1)/(lambda_opt)
lmod3 <- lm(trans_conc ~ solution$hour)
summary(lmod3)
plot(x = solution$hour, y = trans_conc,
     main = "Trans_Concentration vs Time", xlab = "Time", ylab = "Trans_Concentration")
abline(lmod3)

# Q4: Test outliers
set.seed(123)
testdata <- data.frame(x=1:10, y= 6.33-0.024*(1:10) + + rnorm(10, mean=0, sd=0.34))
lmod_true <- lm(y ~ x, testdata)