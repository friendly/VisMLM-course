data("mtcars")

mtcars$cyl <- factor(mtcars$cyl)   # make factors
mtcars$am  <- factor(mtcars$am)

aov1 <- anova(lm(mpg ~ cyl + am, data=mtcars))
aov1

print(aov1, digits=10)             # print with more digits

tidy(aov1)

tidy(aov1, digits=10)

print(tidy(aov1), digits=7)
