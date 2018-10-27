library(ISLR)
?Weekly
summary(Weekly)
pairs(Weekly)
fix(Weekly)
# The only dicernable pattern appears to be from Volume vs Year
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family = binomial)
summary(glm.fit)
attach(Weekly)
# None are statistically significant. Lag2 is the closest
glm.prob = predict(glm.fit)
glm.pred = rep("Down",1089)
glm.pred[glm.prob>=0.5] = "Up"
table(glm.pred,Weekly$Direction)
mean(glm.pred==Weekly$Direction)

# (d) using glm
Weekly.test = Weekly[!train,]
dim(Weekly.test)[1]
train = (Weekly$Year>1990 & Weekly$Year<2008)
Direction.test = Direction[!train]
glm.fittrain = glm(Direction~Lag2,data=Weekly, family = binomial, subset = train)
glm.probs = predict(glm.fittrain, Weekly.test, type="response")
glm.preds = rep("Down", 203)
glm.preds[glm.probs>=0.5] = "Up"
table(glm.preds, Direction.test)
mean(glm.preds==Direction.test)

# (d) using LDA
library(MASS)
lda.fit = lda(Direction~Lag2, data=Weekly, subset=train)
lda.pred = predict(lda.fit, Weekly.test)
table(lda.pred$class, Direction.test)
mean(lda.pred$class==Direction.test)

# (d) using QDA
qda.fit = qda(Direction~Lag2, data=Weekly, subset=train)
qda.pred = predict(qda.fit, Weekly.test)
table(qda.pred$class, Direction.test)
mean(qda.pred$class==Direction.test)

#(d) using KNN with k=1
library(class)
train.X = as.matrix(Lag2)[train,]
test.X = as.matrix(Lag2)[!train,]
train.Direction = Direction[train]
knn.pred = knn(data.frame(train.X), data.frame(test.X), train.Direction, k=1)
table(knn.pred, Direction.test)
mean(knn.pred == Direction.test)

# It is a tie between LDA and GLM

# Playing with the predictors and fit

#Glm
pairs(Weekly)
cor(as.numeric(Direction), Volume)
cor(Weekly[,-9])

glm.try1 = glm(Direction~Lag1*Lag2*Lag3, data=Weekly, subset = train, family = binomial)
summary(glm.try1)
glm.try2 = glm(Direction~log(Lag2), data=Weekly, subset=train, family = binomial)
summary(glm.try2)
glm.probs = predict(glm.try2, Weekly.test)
glm.pred = rep("Down",203)
glm.pred[glm.probs>=0.5]= "Up"
table(glm.pred,Direction.test)
mean(glm.pred==Direction.test)
# Whaaaaat??? AIC is half of the first glm fit but accuracy is lower - small dataset?
AIC(glm.try2,glm.fit)

# QDA
qda.try = qda(Direction~log(Lag2), data=Weekly, subset=train)
qda.pred = predict(qda.fit, Weekly.test)
table(qda.pred$class, Direction.test)
mean(qda.pred$class==Direction.test)
fix(Direction.test)

# KNN
knn.pred = knn(data.frame(train.X), data.frame(test.X), train.Direction, k=50)
table(knn.pred, Direction.test)
mean(knn.pred == Direction.test)
