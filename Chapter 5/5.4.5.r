set.seed(1)
fix(Default)
glm.fit = glm(default~income+balance, data=Default, family = binomial)

train = 1:5000
makefn=function(train){
Default.train = Default[train,]
glm.fitv = glm(default~income+balance, data=Default.train, family = binomial)
glm.probs = predict(glm.fitv, Default[-train,], type="response")
contrasts(Default$default)
glm.pred = rep("No",5000)
glm.pred[glm.probs>0.5] = "Yes"
return(mean(glm.pred!=Default[-train,]$default))
}

?sample
err = rep(0,100)
for (i in 1:100){
  train = sample(10000,5000,replace=FALSE)
  err[i]=makefn(train)
}
mean(err)

makefn=function(train){
  Default.train = Default[train,]
  glm.fitv = glm(default~income+balance+student, data=Default.train, family = binomial)
  glm.probs = predict(glm.fitv, Default[-train,], type="response")
  contrasts(Default$default)
  glm.pred = rep("No",5000)
  glm.pred[glm.probs>0.5] = "Yes"
  return(mean(glm.pred!=Default[-train,]$default))
}

# No major change in test error rate. 0.04% change on avg for 100 runs
