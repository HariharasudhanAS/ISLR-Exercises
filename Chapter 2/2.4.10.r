library(MASS)
?Boston
fix(Boston)
pairs(Boston)
#?sapply
cor(Boston[, 1], Boston[ , sapply(Boston, is.numeric)])
Boston=na.omit(Boston)
apply(Boston,2,range)
apply(Boston,2,mean)
apply(Boston,2,median)
sum(Boston$chas == 1)
dim(Boston)
median(Boston[,"ptratio"])
medir = Boston[order(Boston$medv),]
over8 = subset(Boston, rm>8)
nrow(over8)
summary(over8)
