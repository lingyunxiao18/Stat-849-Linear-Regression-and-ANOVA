# Q1
frog <- data.frame(temp=c(2, 4, 6, 8, 10, 12, 14, 16, 18),
                 heartrate=c(5, 11, 10, 13, 22, 23, 30, 28, 32))

plot(frog$temp,frog$heartrate,
     main='Regression for Heart Rate and Temperature',
     xlab='Temperature',ylab='Heart Rate')

abline(lm(heartrate~temp,data=frog),col='red')

# Q2
power.t.test(n = 9, delta = 1, sd = 2.5, sig.level = 0.05)
power.t.test(n = 9, delta = 1.5, sd = 2.5, sig.level = 0.05)

# Q3
data = read.table("patient.txt", header = TRUE)
hist(data$y, main = 'response')
hist(data$x1, main = 'X1')
hist(data$x2, main = 'X2')
hist(data$x3, main = 'X3')
plot(patient)
cor(patient)
lmod <- lm(y ~ x1 + x2 + x3, patient)
nullmod <- lm(y ~ 1, patient)
summary(lmod2)
anova(nullmod, lmod2)
lmod2 <- lm(y ~ x1 + x3, patient)
anova(lmod3, lmod2)

# HW5 Q2
