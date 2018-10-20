library(ISLR)
?Smarket
summary(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)
summary(glm.fit)$coef[,4]
# When no input is given to probs, it calculates 
# probability of training data : P(Y=1|X)
glm.prob =predict(glm.fit, type = "response")
glm.prob[1:10]
contrasts(Direction)
# So probs calculated probability of going up
glm.pred = rep("Down", 1250)
?rep
glm.pred[glm.prob>0.5] = "Up"
table(glm.pred,Direction)
mean(glm.pred==Direction)
# Create training data - 2001:2004
train = (Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]
glm.fits = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fits, Smarket.2005, type="response")
glm.preds = rep("Down", 252)
glm.preds[glm.probs>0.5]="Up"
table(glm.preds,Direction.2005)
mean(glm.preds==Direction.2005)
glm.fits = glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs = predict(glm.fits, Smarket.2005, type="response")
glm.preds = rep("Down", 252)
glm.preds[glm.probs>0.5]="Up"
table(glm.preds,Direction.2005)
mean(glm.preds==Direction.2005)
predict(glm.fits, newdata = data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)), type="response")

# LDA 
library(MASS)
lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit
plot(lda.fit)
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>=0.5)
lda.pred$posterior[1:20,1]
sum(lda.pred$posterior[,1]>0.9)

# QDA
qda.fit = qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

# KNN
library(class)
?cbind
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred = knn(data.frame(train.X), data.frame(test.X), train.Direction, k=1)
table(knn.pred, Direction.2005)
mean(knn.pred==Direction.2005)
knn.pred = knn(data.frame(train.X), data.frame(test.X), train.Direction, k=3)
mean(knn.pred==Direction.2005)

# Moving on to another dataset : Caravan Insurance Data
?Caravan
summary(Caravan)
attach(Caravan)
summary(Purchase)
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(standardized.X[,1])
test = 1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
# glm
glm.fitting = glm(Purchase~., data=Caravan, family=binomial,subset=-test)
glm.probs=predict(glm.fitting, Caravan[test,],type="response")
glm.pred=rep("No",1000)
glm.pred[glm.probs>0.5]="Yes"
table(glm.pred, test.Y)
glm.pred=rep("No",1000)
glm.pred[glm.probs>0.25]="Yes"
table(glm.pred, test.Y)