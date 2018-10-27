library(ISLR)
fix(Auto)
median(Auto$mpg)
dim(Auto)
mpg01 = rep(0, 392)
mpg01[Auto$mpg>median(Auto$mpg)] = 1
Autou = cbind.data.frame(Auto,mpg01)
names(Autou)
fix(Autou)
attach(Autou)
pairs(Autou)
boxplot(Autou)

# Splitting into train and test
smp_size = floor(0.75 * nrow(Autou))
set.seed(1)
?sample
train_mat = sample(seq_len(nrow(Autou)), size = smp_size)
traindata = Autou[train_mat,]
testdata = Autou[-train_mat,]
fix(traindata)
fix(testdata)

# LDA
??lda
library(MASS)
lda.fit = lda(mpg01~cylinders+weight+displacement+horsepower, data=traindata)
lda.class = predict(lda.fit, testdata)$class
table(lda.class, testdata$mpg01)
mean(lda.class == testdata$mpg01)

# QDA
qda.fit = qda(mpg01~cylinders+weight+displacement+horsepower, data=traindata)
qda.class = predict(qda.fit, testdata)$class
table(qda.class,testdata$mpg01)
mean(qda.class==testdata$mpg01)

# Logistic
glm.fit = glm(mpg01~cylinders+weight+displacement+horsepower, data=traindata, family = binomial)
summary(glm.fit)
glm.probs = predict(glm.fit,  testdata, type = "response" )
glm.pred = rep(0,98)
glm.pred[glm.probs>0.5] = 1
table(glm.pred,testdata$mpg01)
mean(glm.pred == testdata$mpg01)

# KNN
library(class)
train.X = cbind(cylinders,weight,displacement,horsepower)[train_mat,]
test.X = cbind(cylinders,weight,displacement,horsepower)[-train_mat,]
fix(train.X)
mpg01.train = mpg01[train_mat]
knn.pred = knn(data.frame(train.X), data.frame(test.X), mpg01.train, k=100)
table(knn.pred, mpg01[-train_mat])
mean(knn.pred==mpg01[-train_mat])
