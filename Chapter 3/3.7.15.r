library(MASS)
?Boston
lm.fit = lm(crim~. , data=Boston)
# Statistical significance of each variable
anova(lm.fit)
# The others are too long to type