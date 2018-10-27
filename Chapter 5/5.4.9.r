library(MASS)
u =mean(Boston$medv)
sd.err = sd(Boston$medv)/sqrt(dim(Boston)[1])

boot.fn=function(data, index){
  return(mean(data[index,]$medv))
}

boot(Boston,boot.fn,100)
# 95% interval is [21.69, 23.37]
t.test(Boston$medv)
# We get similar results

median(Boston$medv)
boot.fn2=function(data, index){
  return(median(data[index,]$medv))
}

boot(Boston, boot.fn2, 1000)

u.01 = quantile(Boston$medv, probs=c(0.1))

boot.fn3=function(data, index){
  return(quantile(data[index,]$medv, probs=c(0.1)))
}

boot(Boston, boot.fn3, 1000)
