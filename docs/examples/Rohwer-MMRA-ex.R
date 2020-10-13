#' ---
#' title: "Analysis of Rohwer Data with MMRA"
#' author: "Michael Friendly, Matthew Sigal"
#' date: "08 Jun 2016"
#' ---

#' This script reproduces all of the analysis and graphs for the MMRA of the Rohwer data 
#' in the paper and also includes other analyses not described there.  It is set up as an
#' R script that can be "compiled" to HTML, Word, or PDF using `knitr::knit()`.  This is most
#' convenient within R Studio via the `File -> Compile Notebook` option.

#+ echo=FALSE
knitr::opts_chunk$set(warning=FALSE, message=FALSE, R.options=list(digits=4))
options(digits=4)
                      
#' ## Load packages and the data (subsetted)
library(car)
library(heplots)

data("Rohwer", package="heplots")
Rohwer2 <- subset(Rohwer, subset=group==2)
rownames(Rohwer2)<- 1:nrow(Rohwer2) # apply rownames

#' ## fit the multivariate regression model, test hypotheses
#' 
#' Here, we use `linearHypothesis` to test the overall null, ***B = 0***
rohwer.mod <- lm(cbind(SAT, PPVT, Raven) ~ n + s + ns + na + ss, data=Rohwer2)
Anova(rohwer.mlm)
print(linearHypothesis(rohwer.mlm, 
                       c("n", "s", "ns", "na", "ss")), SSP=FALSE)

print(linearHypothesis(rohwer.mlm, 
                       c("n", "s", "ns", "na", "ss")), SSP=FALSE, digits=4)


#' ## View univariate coeffcients:
(rohwer.coef <- coef(rohwer.mlm)[-1,])

#' Alternatively, we can view all coefficients and significance values using the 
#' `tidy()` function in the `broom` package
#' 
library(broom)
tidy(rohwer.mlm)

#' ## Fit separate univariate models models to prepare _Table 1_ in the paper
#' 
#' For a publication-quality summary table of the coefficients, it is most convenient to fit the
#' separate models for each response.
rohwer.mod1 <- lm(SAT   ~ n + s + ns + na + ss, data = Rohwer2)
rohwer.mod2 <- lm(PPVT  ~ n + s + ns + na + ss, data = Rohwer2)
rohwer.mod3 <- lm(Raven ~ n + s + ns + na + ss, data = Rohwer2)

#' ## summary table for univariate models:
#' 
#' The `stargazer` package creates nice tables in HTML, LaTeX or just ASCII text.
#' There are many, many options.
library(stargazer, quietly=TRUE)

stargazer(rohwer.mod1, rohwer.mod2, rohwer.mod3,
          type = "text", digits=2,
          omit.table.layout = "#",
          report = "vc*",
          no.space = TRUE,
          omit.stat = c("n", "adj.rsq", "ser"),
          summary = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          omit = "Constant",
          title="Univariate regression models for Rohwer data")


#' ## Visualize the MMRA model
#' 
#' Set colors to use in the following plots.  They correspond to the **E** ellipse and the **H** ellipses for
#' all hypotheses.
cols <-  c("red", "blue", "black", "darkgreen", "darkcyan", 
           "magenta", "gray20")

#' HE plot, with ellipse to test all 5 regressors
hyp <- list("Regr" = c("n", "s", "ns", "na", "ss"))
heplot(rohwer.mod, 
       hypotheses = hyp,
       fill=TRUE, fill.alpha=0.1, cex=1.5, cex.lab=1.4, col=cols,
       lwd=c(1,3))

#' ## View all pairs of HE plots
pairs(rohwer.mod, hypotheses=hyp, col=cols, var.cex=4,
      fill=TRUE, fill.alpha=0.1, cex=1.5)

#' ## generate a bivariate coefficient plot
#' 
#' This plot shows bivariate confidence regions for the each of the coefficients.
#' Those that don't overlap (0,0) are shaded.
coefplot(rohwer.mod, lwd=2, fill=(1:5) %in% 3:4, 
         main="Bivariate 95% coefficient plot for SAT and PPVT", level=0.95)

#' ## analyze using canonical correlations:
#' 
#' Canonical correlation analysis finds the linear combinations of the _Y_ variables most
#' highly correlated with linear combinations of the _X_s.
#' Only one canonical correlation is significant here, and accounts for 73% of the shared variance 
#' between the predictors and responses, but 100% is accounted for in two dimensions.

library(candisc)
X <- as.matrix(Rohwer2[, 6:10]) # Extract X variables for High SES students
Y <- as.matrix(Rohwer2[, 3:5])  # Extract Y variables for High SES students
cc <- cancor(X, Y, set.names=c("PA", "Ability"))
cc


#' ## visualize canonical correlation output via HE plot

#' This HE plot shows the representation of the regression relations in the space of the first two
#' canonical variates for the Y variables, `Ycan1` and `Ycan2`. The _X_ predictors are shown by their
#' **H** ellipses in this space. The relationship of the _Y_ response variables to their canonical
#' scores is shown by the arrows, reflecting their correlations (structure coefficients) with the 
#' canonical variables.
#' 
#' Note we can also visualize the relationship of joint hypothesis to individual ones
#' in canonical space.

cols <-  c("red", "blue", "black", "darkgreen", "darkcyan", 
           "magenta", "gray20")

heplot(cc, hypotheses=list("na+ns"=c("na", "ns")),
       fill = TRUE, fill.alpha=0.1, col=cols,
       label.pos = c(3, rep(1,5), .1),
       cex=1.4, var.cex=1.25, var.lwd=3, var.col="black")
