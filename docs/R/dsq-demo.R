# Mahalanobis d^2 demo

library(car)
library(heplots)

set.seed(1234)
n <- 100
x1 <- rnorm(n, mean=0, sd = 1)
x2 <- rnorm(n, mean=0, sd = 1)

df1 <- data.frame(x1, x2)

op <- par(mfrow = c(1,2), mar = c(4,4, 2, 0) + .5)
dataEllipse(x1, x2,
            xlim = c(-3, 3),
            ylim = c(-3, 3),
            id = list(n=3, cex=1.2, col="red"),
            asp=1, pch = 16,
            main = paste("Contours of constant D^2, r=", round(cor(x1,x2), 2))
            )

points(df1[c(78, 92, 100),], col="red", pch=16, cex=1.2)
abline(lm(x2 ~ x1, data=df1), col="blue", lwd=2)

#' Correlated case

x1 <- rnorm(n, mean=0, sd = 1)
x2 <- rnorm(n, mean=0, sd = 1) + x1

df2 <- data.frame(x1, x2)

dataEllipse(x1, x2,
            xlim = c(-4, 4),
            ylim = c(-4, 4),
            id = list(n=3, cex=1.2, col="red"),
            asp=1, pch = 16,
            main = paste("Contours of constant D^2, r=", round(cor(x1,x2), 2))
)

points(df2[c(27, 37, 92),], col="red", pch=16, cex=1.2)
abline(lm(x2 ~ x1, data=df2), col="blue", lwd=2)

par(op)


op <- par(mfrow = c(1,2), mar = c(4, 4, 2, 0) + .5)
heplots::cqplot(df1, id.n=3,
                main = paste("Chi-square QQ plot, r=", round(cor(df1$x1,df1$x2), 2)),
                fill.color ="lightblue", fill.alpha=0.05,
                id.col = "red", id.cex=1.3,
                cex.lab = 1.4)

heplots::cqplot(df2, id.n=3,
                main = paste("Chi-square QQ plot, r=", round(cor(df2$x1,df2$x2), 2)),
                fill.color ="lightblue", fill.alpha=0.05,
                id.col = "red", id.cex=1.3,
                cex.lab = 1.4)
par(op)







