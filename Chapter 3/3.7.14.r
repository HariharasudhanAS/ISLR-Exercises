pdf("/home/orange/Desktop/ISLR-Exercises/Chapter 3/3.7.14.pdf")
set.seed(1)
x1 = runif(100)
#?runif
x2 = 0.5*x1+rnorm(100)/10
y = 2+2*x1+0.3*x2+rnorm(100)
cor(x1,x2)
plot(x1,x2)
lm.fit = lm(y~x1+x2)
summary(lm.fit)
# The results are contradictory to those expected
lm.only1 = lm(y~x1)
summary(lm.only1)
# Yes, null hypothesis rejected
lm.only2 = lm(y~x2)
summary(lm.only2)
# Yes, null hypothesis is rejected
x1 = c( x1 , 0.1)
x2 = c( x2 , 0.8)
y = c(y,6)
lm.fit = lm(y~x1+x2)
summary(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit)
dev.off()