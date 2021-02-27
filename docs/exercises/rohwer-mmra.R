#' ---
#' title: 'Exercise: Visualizing MMRA Models'
#' author: "Michael Friendly"
#' date: "2/20/2021"
#' output: 
#'   html_document:
#'     code_folding: "hide"
#' ---
#' 

#' 

#' 

#' 
#' ## Introduction
#' 
#' The purpose of this exercise is to introduce you to some of the basics of:
#' 
#' * fitting multivariate regression models in R,
#' * understanding the difference between univariate and multivariate tests,
#' * data visualization for such models using the `heplots` package
#' 
#' To work through this exercise in R Studio, open a new R script (say, `vismlm-ex2.R`) in the editor and work from there,
#' following this document (in another window).  In this document, you are encouraged to try things yourself, 
#' but you can click <kbd>Code</kbd> / <kbd>Hide</kbd> for individual chunks. When code is shown, the icon at the upper left <img src="clipboard.png" height=22 width=22> will copy it to the clipboard.
#' The script for this excercise is contained in 
#' [exercises/rohwer-mmra.R](exercises/rohwer-mmra.R). 
#' 
#' ### Load packages
#' 
#' It is a good idea to start a script by loading the packages that will be used. We will use the
#' `car`, `heplots`, and `stargazer` packages here.  Load them into your R workspace.
#' (Use `install.packages()` if any are not found.)
#' 
## ----loadpkgs, class.source = 'fold-show'------------------------------------------------------------------------------------
library(car)
library(heplots)
library(stargazer)

#' 
#' ### Rohwer data
#' 
#' This example concerns regression analysis predicting measures of aptitude and achievement for a sample of **Low SES** kindergarten students from measures of performance on paired associate memory tests.  
#' The response variables are:
#' 
#' *	`SAT` – A scholastic aptitude test
#' *	`PPVT` – Peabody picture vocabulary test
#' *	`Raven` – the Raven progressive matrices test
#' 
#' The predictors are scores on 5 paired associate tasks called `N, S, NS, NA, SS`.
#' These differ in the way the stimulus term is presented: 
#' 
#' * `N` = named; 
#' * `S` = used in a 'still' task; 
#' * `NS` = used in a named still task; etc.
#' 
#' See: `help("Rohwer", package="heplots")` for more details.
#' 
#' ### Load the data
#' 
#' The dataset `Rohwer` contains 69 observations from two groups of kindergarden children. A group of 37 children from a
#' a High-SES neighborhood (`SES=='Hi'`) and a group of 32 children from a Low-SES neighborhood (`SES=='Lo'`).
#' For the purposes of this exercise, use the following code to load the data and select only the **Low SES** group,
#' giving the data.frame `Rohwer2`.
#' 
## ----read-rohwer, class.source = 'fold-show'---------------------------------------------------------------------------------
data("Rohwer", package="heplots")
Rohwer2 <- subset(Rohwer, subset=SES=='Lo')
rownames(Rohwer2)<- 1:nrow(Rohwer2)

#' 
#' ## Univariate regression models
#' 
#' A univariate approach entails fitting a separate regression model for each of the `PPVT`, `SAT`, and `Raven` variables.
#' This code fits & tests the model for `SAT`.
#' 
## ----mod1, class.source = 'fold-show'----------------------------------------------------------------------------------------
rohwer.mod1 <-lm(SAT ~ n + s + ns + na + ss, data=Rohwer2)
Anova(rohwer.mod1)

#' 
#' 
#' 
#' Fit and test an analogous model `rohwer.mod2` for the `PPVT` score. I'm showing here the output you should see.
#' 
## ----mod2, class.source = 'fold-hide'----------------------------------------------------------------------------------------
rohwer.mod2 <-lm(PPVT ~ n + s + ns + na + ss, data=Rohwer2)
Anova(rohwer.mod2)

#' 
#' Do the same to give `rohwer.mod3` for  `Raven`.
#' 
## ----mod3, class.source = 'fold-hide'----------------------------------------------------------------------------------------
rohwer.mod3 <-lm(Raven ~ n + s + ns + na + ss, data=Rohwer2)
Anova(rohwer.mod3)

#' 
#' ### Interpretation
#' 
#' Review the Anova summaries above.
#' What do you conclude for these response variables?  In particular,
#' 
#' * Which PA tests are significant predictors for **each** response variable?
#' * Which PA tests are significant predictors for **all** response variables?
#' 
#' For presentation, or for ease of direct comparison, it is easier to combine the model
#' summary statistics into a table.  `stargazer` is one package that does this,
#' and can produce output in text, LaTeX or HTML format.
#' There are many, many options.
#' 
## ----unimods, class.source = 'fold-show'-------------------------------------------------------------------------------------
stargazer(rohwer.mod1, rohwer.mod2, rohwer.mod3,
	type = "text", 
	digits=2,
	star.cutoffs = c(0.05, 0.01, 0.001),
	omit = "Constant",
	no.space=TRUE,
	title="Univariate regression models for Rohwer data")

#' 
#' Re-evaluate your conclusions from these univariate models.
#' 
#' ## Multivariate model
#' 
#' Fit the multivariate regression model for all three responses.
#' The syntax for a multivariate `lm()` uses `cbind(Y1, Y2, ...)` to "bind" several responses together.
#' 
## ----rohwer-mlm, class.source = 'fold-hide'----------------------------------------------------------------------------------
rohwer.mlm <- lm(cbind(SAT, PPVT, Raven) ~ n + s + ns + na + ss, data=Rohwer2)
Anova(rohwer.mlm)


#' 
#' ## Overall tests
#' 
#' For a univariate model, the model $F$-test gives an overall test 
#' of the hypothesis that **all** regression coefficients are zero,
#' i.e., none of the predictors explain the response beyond chance.
#' 
#' `car::linearHypothesis()` is more general. It can be used to test 
#' any hypothesis regarding subsets of predictors or their linear
#' combinations.
#' For convenience, create a vector of the names of the `predictors`
#' for an overall test.
#' 
#'  
#' 
## ----lh1, class.source = 'fold-show'-----------------------------------------------------------------------------------------
predictors <- c("n", "s", "ns", "na", "ss")
linearHypothesis(rohwer.mod1, hypothesis=predictors)

#' 
#' This is equivalent to the test of the hypothesis that $R^2_{SAT} = 0$.
#' 
#' You can try doing the same test for `PPVT` in `rohwer.mod2`, and for `Raven` in `rohwer.mod3`.
#' What do you conclude?
#' 
## ----lh2---------------------------------------------------------------------------------------------------------------------
linearHypothesis(rohwer.mod2, hypothesis=predictors)

#' 
## ----lh3---------------------------------------------------------------------------------------------------------------------
linearHypothesis(rohwer.mod3, hypothesis=predictors)

#' 
#' 
#' 
#' ## Overall multivariate test
#' 
#' Try `linearHypothesis()` using the multivariate model, `rohwer.mlm`. This tests the multivariate hypothesis that
#' $\mathbf{B}_{3 \times 4} = [ \mathbf{\beta}_{SAT}, \mathbf{\beta}_{PPVT}, \mathbf{\beta}_{Raven}] = \mathbf{0}$.
#' 
## ----lh-mlm, class.source = 'fold-hide'--------------------------------------------------------------------------------------
predictors <- c("n", "s", "ns", "na", "ss")
linearHypothesis(rohwer.mlm, hypothesis=predictors)

#' 
#' 
#' 
#' ## HE plots: Visualizing the multivariate tests 
#' 
#' A basic HE plot shows the relation of **all** predictors to a pair of response variables (the first two, by default).
#' In MMRA models, individual predictors have 1 df, so their **H** ellipses plot as degenerate lines.
#' 
#' * The relative size of any **H** ellipse reflects the strength (*effect size*) of that predictor for the
#' two response variables shown in the plot.
#' 
#' * By default, these are scaled to show *significance*. The general rule is that any predictor is significant (using Roy's test) if its **H** ellipse (line) protrudes *anywhere* outside the **E** ellipse.  
#' 
#' * The orientation of an **H** ellipse shows how a given
#' predictor is associated with both response variables. E.g., the positive slope for `ns`  indicates that it is
#' associated with higher scores on *both* `SAT` and `PPVT`.
#' 
## ----he1, class.source = 'fold-show'-----------------------------------------------------------------------------------------
heplot(rohwer.mlm, fill = TRUE)

#' 
#' We can add the **H** ellipse to show the overall test for all predictors, using the `hypothesis=` argument.
#' There are many options for `heplot()`.
#' 
## ----he2, class.source = 'fold-show'-----------------------------------------------------------------------------------------
cols <-  c("red", "blue", "black", "darkgreen", "darkcyan", "magenta", 
            "gray50")
hyp <- list("Regr" = c("n", "s", "ns", "na", "ss"))
heplot(rohwer.mlm, 
       hypotheses=hyp, 
       fill = TRUE, fill.alpha=.10,
       cex = 1.4,
       col = cols)

#' 
#' Note how the orientation of the overall `Regr` ellipse reflects the contributions of `ss` and `ns`, aligned mainly
#' with the `SAT` score, and `s` and `na` aligned mainly with the `PPVT` score.
#' 
#' 
#' ### `pairs()` plot
#' 
#' We can also view all pairwise HE plots using the `pairs.mlm` method.
#' 
## ----he-pairs, class.source = 'fold-show'------------------------------------------------------------------------------------
pairs(rohwer.mlm, 
      hypotheses=hyp, 
      fill = TRUE,
      col = cols)

