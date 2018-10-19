library(car)
?Carseats
pdf("/home/orange/Desktop/ISLR-Exercises/Chapter 3/3.7.10.pdf")
lm.fit = lm(Sales~Price+Urban+US, data=Carseats)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)
par(mfrow=c(1,1))
# Using only predictors with small p-value
lm.fit2 = lm(Sales~Price+US, data= Carseats)
summary(lm.fit2)
anova(lm.fit,lm.fit2)
confint(lm.fit2)
# yes, there are high leverage points
dev.off()
# Epilogue
lm.fitall = lm(Sales~., data=Carseats)
summary(lm.fitall)
