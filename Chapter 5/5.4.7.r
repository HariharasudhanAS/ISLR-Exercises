glm.fit = glm(Direction~Lag1+Lag2, data=Weekly, family = binomial)

glm.fit = glm(Direction~Lag1+Lag2, data=Weekly[-1,], family = binomial)
predict(glm.fit, Weekly[1,] , type="response")

# No, the prediction was wrong
err = rep(0, dim(Weekly)[1])
for( i in 1:dim(Weekly)[1]){
  glm.fit = glm(Direction~Lag1+Lag2, data=Weekly[-i,], family = binomial)
  glm.prob = predict(glm.fit, Weekly[i,] , type="response")
  if(glm.prob>0.5)
  {
    x = "Up"
  } else {
    x = "Down"
  }
  if(x!=Weekly[i,]$Direction)
  {
    err[i] = 1
  }
}
sum(err)
mean(err)
