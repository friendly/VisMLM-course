# from: https://stats.stackexchange.com/questions/565567/visualizing-multivariate-multiple-regression-of-continuous-data-in-r
set.seed(0)

df <- data.frame(ind1 = c(1:10),
                 ind2 = runif(10,5,15),
                 ind3 = runif(10,5,15))
df$dep1 <- df$ind1 * df$ind2 * df$ind3
df$dep2 <- df$ind1 * df$ind2 * df$ind3 * runif(10)
df$dep3 <- df$ind1 * df$ind2 * df$ind3 * abs(rnorm(10))

model1 <- lm(data = df, dep1 ~ ind1 + ind2 + ind3)

model2 <- lm(data = df, cbind(dep1, dep2, dep3) ~ ind1 + ind2 + ind3)

car::Anova(model2)

library(heplots)
library(candisc)
heplot(model2, fill=TRUE, fill.alpha=0.1)

pairs(model2, , fill=TRUE, fill.alpha=0.1)


mod.can <- candisc(model2)
mod.can

heplot(mod.can)

# from one answer
# One option is to create a 3D surface plot showing the sensitivity of your dependent variables to the independent variables in your model.

df2 <- data.frame(ind1 = seq(from = min(df$ind1), to = max(df$ind1), by = (max(df$ind1)-min(df$ind1))/9),
                  ind2 = seq(from = min(df$ind2), to = max(df$ind2), by = (max(df$ind2)-min(df$ind2))/9),
                  ind3 = seq(from = min(df$ind3), to = max(df$ind3), by = (max(df$ind3)-min(df$ind3))/9))
# generate predictions
tst <- predict(model2, df2)

library(plotly)

# generate a numeric matrix for use with plotly::plot_ly
mat <- matrix(ncol = ncol(tst), nrow = nrow(tst))

# populate matrix with numeric values from model prediction
for (i in 1:3) {
  mat[,i] <- tst[,i]
}

# generate 3D surface
plot_ly(type = 'surface', z = mat) %>%
  # customize axis titles
  layout(scene = list(xaxis = list(title = 'dep1'),
                      yaxis = list(title = 'dep2'),
                      zaxis = list(title = 'dep3')))


