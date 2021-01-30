#' ---
#' title: 'Exercise: Visualizing Regression Models with R'
#' author: "Michael Friendly"
#' date: "1/20/2021"
#' output: 
#'   html_document:
#'     code_folding: "hide"
#' ---
#' 

#' 

#' 

#' 
#' # Introduction
#' 
#' The purpose of this exercise is to introduce you to some of the basics of:
#' 
#' * fitting linear models in R,
#' * data visualization for such models,
#' * regression diagnostics to judge the adequacy of the statistical methods.
#' 
#' To work through this exercise in R Studio, open a new R script (say, `vismlm-ex1.R`) in the editor and work from there,
#' following this document (in another window).  In this document, you are encouraged to try things yourself, 
#' but you can click <kbd>Code</kbd> / <kbd>Hide</kbd> for individual chunks. When code is shown, the icon at the upper left will copy it to the clipboard.
#' The script for this excercise is contained in 
#' [exercises/duncan-ex1.R](exercises/duncan-reg.R). 
#' 
#' ### Load packages
#' 
#' It is a good idea to start a script by loading the packages that will be used. We will use the
#' `car` and `effects` packages here.  Load them into your R workspace.
#' 
## ----loadpkgs, class.source = 'fold-show'------------------------------------------------------------------------------------
library(car)
library(effects)

#' 
#' 
#' ## Data
#' 
#' The data used here come from a study by O. D. Duncan (1961), on the relation between `prestige`
#' of occupations and the average `income` and `education` of those jobs. The variable
#' `type` classifies the occupations as: 
#' 
#' * blue collar (`bc`)
#' * white cholar (`wc`)
#' * professional (`prof`)
#' 
#' Here are just a few of the observations (using `car::some()`):
#' 
## ----Duncan, class.source = 'fold-show'--------------------------------------------------------------------------------------
data(Duncan, package="carData")
car::some(Duncan)

#' 
#' NB: This data set is similar in scope to the `carData::Prestige` data used in the lecture, but
#' the quantitative variables (`income`, `education`, `prestige`) here are scaled as percents---
#' percent of those in a given occupation with a given level of income, education or prestige.
#' 
#' 
#'   1. The data is in the `carData` package. Read it in with:
## ----read-duncan, eval=FALSE, class.source = 'fold-show'---------------------------------------------------------------------
## library(car)
## data(Duncan, package="carData")

#' 
#' ## Graphic overviews
#' 
#'   2. As always, it is useful to get an overview of the data by plotting.  We should look for indications that relationships are seriously nonlinear or other oddities. Use the `car::scatterplotMatrix()` function to produce a reasonable plot of `prestige`, `income` and `education`.
## ----spm, h=6, w=6, class.source = 'fold-show'-------------------------------------------------------------------------------
scatterplotMatrix(~prestige + income + education, data=Duncan, 
                  id=list(n=2))

#' 
#' There is something unusual about the marginal distributions of the three variables. What do you think is behind this?
#' 
#' The scatterplot matrix above can be made much more useful with some of the many options available.
#' Use `help(scatterplotMatrix)` (or `?scatterplotMatrix`) to see the options.
#' 
#' * My preferred version uses data ellipse (`ellipse = list(...)`) and suppresses the confidence band for the loess smoother (`smooth = list(spread=FALSE, ...)`). What can you do to approximate this?
#' 
## ----spm2, h=6, w=6----------------------------------------------------------------------------------------------------------
scatterplotMatrix(~ prestige + education + income, data=Duncan,
                  id = list(n=2),
                  regLine = list(method=lm, lty=1, lwd=2, col="black"),
                  smooth=list(smoother=loessLine, spread=FALSE,
                              lty.smooth=1, lwd.smooth=3, col.smooth="red"),
                  ellipse=list(levels=0.68, fill.alpha=0.1))


#' 
#' * We haven't used `type` of occupation in this analysis yet. Use `| type` in the model formula
#' to condition on occupation type.
#' 
## ----spm3, h=6, w=6----------------------------------------------------------------------------------------------------------
scatterplotMatrix(~ prestige + education + income | type ,
                  data=Duncan, smooth=FALSE)

#' 
#' 
#' ## Fitting models
#' 
#'   3.	Proceed to fit a model using both income and education as predictors.  What do you conclude from this? How would you interpret the coefficients in the model?
#' 
## ----duncan.mod, class.source = 'fold-show'----------------------------------------------------------------------------------
duncan.mod <- lm(prestige ~ income + education, data=Duncan)
summary(duncan.mod)

#' 
#' ### Plotting residuals
#' 
#'   4.	Examine the four plots produced by `plot(duncan.mod)`.  What do they indicate about possible problems with the model? 
#'   
## ----plot-duncan-mod, h=6, w=6-----------------------------------------------------------------------------------------------
par(mfrow = c(2,2))
plot(duncan.mod)

#' 
#'   5. Other plots of residuals from the `car` package are more useful and more flexible.  For well-behaved data they should all look unstructured, with no systematice patterns.  Try a few of the methods illustrated in the lecture.
#' 
## ----resid-plots-------------------------------------------------------------------------------------------------------------
residualPlots(duncan.mod, id=list(n=2))

#' 
#' 
#' <!-- These all look fairly OK.  The "Scale-Location" plot shown earlier does not -->
#' 
#' <!-- ```{r} -->
#' <!-- spreadLevelPlot(duncan.mod) -->
#' <!-- ``` -->
#' 
#' 
#' 
#' 
#'   6.	For today, we are interested mainly in whether there are any highly influential observations in the data --– those that could change the regression coefficients depending on whether they were included or omitted.  Probably the single most useful plot you can make is an “influence plot” that shows the residual and leverage, with bubbles proportional to a measure of total influence –-- Cook’s D statistic.
#' 
## ----inflplot----------------------------------------------------------------------------------------------------------------
influencePlot(duncan.mod, id=list(n=2))

#' 
#' In the plot above, which observation is most influential? Which observation has the greatest leverage?
#' 
#' ## Added-variable plots
#' 
#'   7. *added variable* plots	(aka: *partial residual* plots) are very useful, because they show the **unique** partial relationship of Y to each X, controlling (or adjusting) for all other predictors. In these plots "adjusting" means that `prestige | others` is actually the residual for `prestige` in the regression from the other variable.
#' 
#' The `{car}` function is `avPlots()`.  Try this.
#' 
## ----avplot, h=4, w=8--------------------------------------------------------------------------------------------------------
avPlots(duncan.mod, id=list(n=2))

#' 
#' Several observations stand out in both the influence plot and in the partial residual plots, and were identified in the plots by their labels (id.n= ).  Try to think why these might be unusual in terms of the context of this data.
#' 
#' ## Effect plots
#' 
#' Effect plots are similar in spirit to added variable plots, in that they attempt to show the relationship of
#' the outcome to a focal predictor with other predictors controlled.
#' They differ in that, for a given focal predictor (shown in the plot), all other predictors in the model
#' are averaged over by being set equal to their means (or medians).
#' 
#' In the `{effects}` package, `allEffects()` calculates the effect values for all predictors in the model;
#' `predictorEffects()` is similar. Try this.
#' 
#' 
## ----effplot, , h=4, w=8-----------------------------------------------------------------------------------------------------
mod.effects <- allEffects(duncan.mod, residuals=TRUE)
plot(mod.effects, id=list(n=4, col="black"))

#' 
#'   
