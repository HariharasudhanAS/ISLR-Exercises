# To ensure reproducibility
set.seed(1)
x=rnorm(100)
y = 2*x+rnorm(100)
lm.fit = lm(y~x+0)
summary(lm.fit)
lm.fit2 = lm(x~y+0)
summary(lm.fit2)
# AIC(lm.fit2,lm.fit)
lm.fit3 = lm(y~x)
lm.fit4 = lm(x~y)
summary(lm.fit3)
summary(lm.fit4)
# The t-value is the same for x onto y and y onto x