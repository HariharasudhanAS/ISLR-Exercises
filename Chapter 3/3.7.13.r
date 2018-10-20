pdf("/home/orange/Desktop/ISLR-Exercises/Chapter 3/3.7.13.pdf")
set.seed(1)
x = rnorm(100)
#?rnorm
eps = rnorm(100, 0, 0.5)
y = -1 + 0.5*x + eps
plot(x,y)
lm.fit = lm(y~x)
summary(lm.fit)
# the sample coeff are close to the population coeff
abline(lm.fit, col=2)
abline(-1,0.5, col=3)
#?legend
legend(-1.5, legend = c("model fit", "pop. regression"), col=2:3, lwd=2)
par(mfrow=c(2,2))
plot(lm.fit)
lm.poly = lm(y~I(x^2)+x)
anova(lm.fit,lm.poly)
# Not much improvement in the model, p>0.05 thus not enough evidence
# against null hypothesis that the models are equally good
eps = rnorm(100, 0, 0.25)
y = -1 + 0.5*x + eps
lm.fit2 = lm(y~x)
summary(lm.fit2)
par(mfrow=c(1,1))
plot(x,y)
abline(lm.fit2, col=2)
abline(-1,0.5, col=3)
legend(-1.5, legend = c("model fit", "pop. regression"), col=2:3, lwd=2)
# p-value has decreased and R squared value increased
# p-value increases and R squared value decreases
# higher variance in confidence in interval
confint(lm.fit)
confint(lm.fit2)
dev.off()