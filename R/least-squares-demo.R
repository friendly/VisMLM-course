# demo least squares

set.seed(1234)
n <- 50
X <- scale(runif(n, -3, 3), center=TRUE, scale=FALSE)
Y <- scale(X + rnorm(n, 0, 2), center=TRUE, scale=FALSE)
df <- data.frame(X,Y)
mod <- lm(Y ~ X, data=df)
summary(mod)

b0 <- coef(mod)[1]
b1 <- coef(mod)[2]

df$fit <- predict(mod)
df$resid <- Y - df$fit

head(df)

dfs <- NULL
slope <- seq(0, 1, by = 0.1)
for (i in 1:length(slope)) {
  fit   <- b0 + slope * df$X
  resid <- df$Y - fit
  new <- data.frame(slope, X = df$X, Y, fit, resid)
  dfs <- rbind(dfs, new)
}
str(dfs)


library(ggplot2)
library(gganimate)



