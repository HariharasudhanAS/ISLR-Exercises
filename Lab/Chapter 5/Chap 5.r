library(ISLR)
set.seed(1)

#random selection
train = sample(392,196)

# model fitting
lm.fit = lm(mpg~horsepower, data=Auto, subset=train)
summary(lm.fit)
attach(Auto)

# MSE in test set
mean((mpg-predict(lm.fit, Auto))[-train]^2)

# testing for quadratic and cubic
lm.fit2 = lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3 = lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# Different sample
set.seed(2)
train = sample(392, 196)
lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)
lm.fit2 = lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3 = lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# LOOCV
library(boot)
?cv.glm # default LOOCV
glm.fit = glm(mpg~horsepower, data=Auto)
cv.err = cv.glm(Auto, glm.fit)
cv.err$delta
library(ISLR)
set.seed(1)

#random selection
train = sample(392,196)

# model fitting
lm.fit = lm(mpg~horsepower, data=Auto, subset=train)
summary(lm.fit)
attach(Auto)

# MSE in test set
mean((mpg-predict(lm.fit, Auto))[-train]^2)

# testing for quadratic and cubic
lm.fit2 = lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3 = lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# Different sample
set.seed(2)
train = sample(392, 196)
lm.fit=lm(mpg~horsepower, data=Auto, subset=train)
mean((mpg-predict(lm.fit, Auto))[-train]^2)
lm.fit2 = lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
lm.fit3 = lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)

# LOOCV
library(boot)
?cv.glm # default LOOCV
glm.fit = glm(mpg~horsepower, data=Auto)
cv.err = cv.glm(Auto, glm.fit)

cv.error = rep(0,5)
for ( i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
print(cv.error)

set.seed(17)
cv.error.10 = rep(0,5)
for ( i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit, K=10)$delta[1]
}
print(cv.error.10)
# Note:
# (In principle, the computation time for LOOCV for a least squares linear
# model should be faster than for k-fold CV, due to the availability of the
# formula (5.2) for LOOCV (inflated MSE); however, unfortunately the cv.glm() function
# does not make use of this formula.)

# Bootstrap
alpha.fn = function(data, index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
#Test fn
alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))

boot(Portfolio,alpha.fn,R=1000)

boot.fn=function(data,index){
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
}
boot.fn(Auto, sample(392,392,replace=T))
boot(Auto,boot.fn,R=1000)

boot.fn=function(data, index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data = Auto ) )$coef
