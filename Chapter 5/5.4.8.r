set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

# n is 100 and p is 1
plot(x,y)
# We obtain a inverted parabola

set.seed(12)
Data = data.frame(x,y)
fix(Data)

glm.fit1 = glm(y~x, data=Data)
cv.err1 = cv.glm(Data, glm.fit1)$delta
cv.err1

glm.fit2 = glm(y~x+I(x^2), data=Data)
cv.err2 = cv.glm(Data, glm.fit2)$delta
cv.err2

glm.fit3 = glm(y~x+I(x^2)+I(x^3), data=Data)
cv.err3 = cv.glm(Data, glm.fit3)$delta
cv.err3

glm.fit4 = glm(y~x+I(x^2)+I(x^3)+I(x^4), data=Data)
cv.err4 = cv.glm(Data, glm.fit4)$delta
cv.err4

