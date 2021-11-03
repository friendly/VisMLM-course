---
  title: "more diagnostic plots for linear models"
output:
  html_output
---
  
#' This uses the `check_model` function from the `performance` package
#' 
data(Prestige, package="carData")
Prestige$type <- ordered(Prestige$type, levels=c("bc", "wc", "prof")) # reorder levels

mod0 <- lm(prestige ~ education + income + women + type,
           data=Prestige)

if(!require(performance)) install.packages("performance")   # also needs "see"

library(performance)

#' the default plot gives an array of six nicely done diagnostic plots
check_model(mod0)

# can specify checks="all", or a subset of these
check <- c("vif", "qq", "normality", "linearity", "ncv", "homogeneity", "outliers")

check_model(mod0, check=c("normality", "linearity", "ncv", "homogeneity", "outliers"))

#' detrended QQ plots are often better
#' 
check_model(mod0, check="qq", detrend=TRUE)



