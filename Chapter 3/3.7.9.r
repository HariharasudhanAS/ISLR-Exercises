library(ISLR)
pdf("/home/orange/Desktop/ISLR-Exercises/Chapter 3/3.7.9.pdf")
pairs(Auto)
fix(Auto)
names(Auto)
Auto = Auto[,-9]
fix(Auto)
cor(Auto)
lm.fit=lm(mpg~., data = Auto)
summary(lm.fit)
# Yes, there is a relation between the predictors and response
# displacement, weight, year, origin
# mpg increases with newer cars
par(mfrow=c(2,2))
plot(lm.fit)
# the residuals vs fitted has a pattern, suggesting non-linear fit
# observation 14 has a very large leverage 
lm.try1 = lm(mpg~displacement+weight+year+origin, data=Auto)
anova(lm.try1,lm.fit)
lm.try2 = lm(mpg~displacement:weight+weight+year+origin+I(displacement^0.5), data=Auto)
summary(lm.try2)
lm.try3 = lm(mpg~displacement*weight+year+origin, data=Auto)
summary(lm.try2)
anova(lm.try2,lm.try3)
lm.try4 = lm(mpg~displacement*weight+year+origin+horsepower, data = Auto)
anova(lm.try3,lm.try4)
summary(lm.try4)
lm.try5 = lm(mpg~displacement*weight+year+origin+I(horsepower^0.5), data = Auto)
anova(lm.try4,lm.try5)
summary(lm.try5)
# Minor improvements are seen if the variables are transformed, while interaction effect impacts the models in a larger way
# Trying out comparision test:
?AIC
AIC(lm.try5, lm.try1, k=2)
anova(lm.try5)
dev.off()
