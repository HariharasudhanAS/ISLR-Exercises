set.seed(1)
x = rnorm(100)
y = 2*x
lm.fit1 = lm(x~y)
lm.fit2 = lm(y~x)
summary(lm.fit1)
summary(lm.fit2)
y = x+2
lm.fit1 = lm(x~y)
lm.fit2 = lm(y~x)
summary(lm.fit1)
summary(lm.fit2)