#' ---
#' title: "Coefficient plots for linear models"
#' author: "Michael Friendly"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     theme: readable
#'     code_download: true
#' ---

#+ echo=FALSE
knitr::opts_chunk$set(warning=FALSE, message=FALSE, R.options=list(digits=4))

#' Coefficient plots are often more useful than tables
#' but plotting _raw coefficients_ can be misleading when the predictors are
#' on different scales.
#' 
#' The packages `arm` and `modelsummary` are used to illustrate these plots,
#' discovering some other problems with naive use of coefficient plots.
#' I compare plotting:
#'
#' * raw coefficients
#' * standardized coefficients
#' * meaningfully scaled coefficients
#' 

#' ### Load packages
library(arm)           # coefplot
library(modelsummary)  # model plot
library(dplyr)
library(ggplot2)


#' ## Occupational prestige data
#' How does occupational prestige depend on education (years), income ($), %women, ...?
#' 
#' I use a classic example from the `carData` package. The observations are averages for occupational
#' categories rather than individuals. The data come from the 1971 Canadian census.
data(Prestige, package="carData")
head(Prestige, 5)

#' ## Fit a simple model
mod0 <- lm(prestige ~ education + income + women,
           data=Prestige)
summary(mod0)

#' ## Visualize coefficients
#' The plots below show default coefficient plots for this model. 
#' 
#' They are disappointing and misleading because these show the raw coefficients. 
#' It looks like only `education` has a non-zero effect. It is misleading because the predictors
#' are on different scales, so it makes little sense to compare the change in `prestige` for
#' a 1 year increase in `education` with the change for a $1 increase in `income`.
modelplot(mod0, coef_omit="Intercept", color="blue", size=1) +
  labs(title="Raw coefficients for mod0")

arm::coefplot(mod0, col.pts="red", cex.pts=1.5)

#' ## Fit a more complex model
#' Model `log(income)` and add an interaction with `type` of occupation.
mod1 <- lm(prestige ~ education + women +
             log(income)*type, data=Prestige)
summary(mod1)

#' ## Compare models
#' Several packages allow to compare several models in the same plot.
arm::coefplot(mod0, col.pts="red", cex.pts=1.5)
arm::coefplot(mod1, add=TRUE, col.pts="blue", cex.pts=1.5)

#' But **WAIT**: `income` was entered as `log(income)` in `mod1`. It was stupid to
#' try to compare the coefficients, but this should have raised an error.
#' 

#' ## Standardize the variables, giving $\beta$ coefficients
#' An alternative is to present the standardized coefficients. These are interpreted
#' as the standardized change in prestige for a one standard deviation change in the
#' predictors.
#' 
#' One way to do this is to transform all variables to standardized ($z$) scores.
#' The syntax ultimately uses `scale` to transform all the numeric variables.
#' 

Prestige_std <- Prestige %>%
  as_tibble() %>%
  mutate(across(where(is.numeric), scale))

#' ### Fit the same models to the standardized variables
#' This accords better with the fitted (standardized) coefficients. `income` and `education`
#' are now both large and significant compared with the effect of `women`.
#' 
#' But a nagging doubt remains:  How can I interpret the numerical **size** of coefficients?

mod0_std <- lm(prestige ~ education + income + women,
               data=Prestige_std)

mod1_std <- lm(prestige ~ education + women +
             log(income)*type, 
           data=Prestige_std)
#summary(mod1_std)

#' ## Plot the standardized coefficients
#' These plots look more like what I was expecting to show the relative magnitudes of the coefficients
#' and CIs so one could see which differ from zero.
#' 
modelplot(mod0_std, coef_omit="Intercept", color="blue", size=1) +
  labs(title="Standardized coefficients for mod0")

arm::coefplot(mod0_std, col.pts="red", cex.pts=1.5)
arm::coefplot(mod1_std, add=TRUE, col.pts="blue", cex.pts=1.5)




#' ## More meaningful units
#' A better substantative comparison of the coefficients could use understandable scales for the
#' predictors, e.g., months of education, $50,000 income or 10% of women's participation.
#' 
#' Note that the effect of this is just to multiply the coefficients and their standard errors by a factor. 
#' The statistical conclusions of significance are unchanged.

Prestige_scaled <- Prestige %>%
  mutate(education = 12 * education,
         income = income / 50,
         women = women / 10)

mod0_scaled <- lm(prestige ~ education + income + women,
               data=Prestige_scaled)

summary(mod0_scaled)

arm::coefplot(mod0_scaled, col.pts="red", cex.pts=1.5,
              main = "Coefficients for prestige with scaled predictors",
              varnames=c("intercept", 
                         "education\n(/month)",
                         "income\n(/$50K)",
                         "women\n(/10%)")
               )

