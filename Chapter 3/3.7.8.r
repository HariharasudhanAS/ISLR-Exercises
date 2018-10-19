library(ISLR)
pdf("/home/orange/Desktop/3.7.8.pdf")
lm.fit = lm(mpg~horsepower, data=Auto)
summary(lm.fit)
confint(lm.fit, level = 0.95)
predict(lm.fit, data.frame( horsepower = c(98)), interval = "prediction")
plot(Auto$horsepower,Auto$mpg)
abline(lm.fit, col="red")
cor(Auto$horsepower,Auto$mpg)
par(mfrow=c(2,2))
plot(lm.fit)
dev.off()


