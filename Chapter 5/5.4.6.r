set.seed(1)
glm.fit = glm(default~income+balance, data=Default, family=binomial)
summary(glm.fit)

boot.fn = function(data, index){
  coefficients(glm(default~income+balance, data=data, family=binomial, subset=index))
}

boot(Default,boot.fn,1000)

# The results are very similar