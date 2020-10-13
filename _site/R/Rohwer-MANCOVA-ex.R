#' ---
#' title: "Analysis of Rohwer Data with MANCOVA"
#' author: "Michael Friendly, Matthew Sigal"
#' date: "08 Jun 2016"
#' ---

#' This script reproduces all of the analysis and graphs for the MANCOVA of the Rohwer data 
#' in the paper and also includes other analyses not described there.  It is set up as an
#' R script that can be "compiled" to HTML, Word, or PDF using `knitr::knit()`.  This is most
#' convenient within R Studio via the `File -> Compile Notebook` option.

#+ echo=FALSE
knitr::opts_chunk$set(warning=FALSE, message=FALSE, R.options=list(digits=4))


#' ## Load packages and the data
library(heplots)
data(Rohwer, package="heplots")

#' ## Fit the MANCOVA model, test hypotheses:
#' 
#' `SES` is the group factor, representing `Hi` and `Lo` socioeconomic status
rohwer.mod <- lm(cbind(SAT, PPVT, Raven) ~ SES + n + s + ns + na + ss,
                 data=Rohwer)
Anova(rohwer.mod)

#' ## pairwise HE plots
pairs(rohwer.mod, 
      hypotheses=list("Regr" = c("n", "s", "ns", "na", "ss")), 
      fill=TRUE, fill.alpha=0.1)

#' ## Fit heterogeneous regression model with SES interactions:
rohwer.mod1 <- lm(cbind(SAT, PPVT, Raven) ~ SES * (n + s + ns + na + ss), 
                  data=Rohwer)
Anova(rohwer.mod1)

#' ## Assess significance of interactions
#' 
#' `linearHypothesis()` takes a vector of  terms to test simulataneously. 
#' Here we use `grep()` to find all interaction terms, which have a ":" in
#' their name.
coefs <- rownames(coef(rohwer.mod1))   # store coefficient names in a vector
print(linearHypothesis(rohwer.mod1,    # only test for interaction effects
                       coefs[grep(":", coefs)]), SSP=FALSE) 

#' ## Alternatively, fit models for each group separately:
#' 
#' This also allows us to see possible differences in within-group error variances and
#' covariances
rohwer.ses1 <- lm(cbind(SAT, PPVT, Raven) ~ n + s + ns + na + ss, 
                  data = Rohwer, subset = SES == "Hi")
rohwer.ses2 <- lm(cbind(SAT, PPVT, Raven) ~ n + s + ns + na + ss, 
                  data = Rohwer, subset = SES == "Lo")

#' ## Visualize results with overlaid HE plots:
#' 
#' For this to work correctly, it is necessary to set the x,y axis limits the
#' explicitly to be the _same_ in both plots

## Low SES students:
heplot(rohwer.ses2, col = c("red", rep("black",5), "blue"), 
       hypotheses = list("B=0, Low SES" = c("n", "s", "ns", "na", "ss")), 
       level = 0.5, cex = 1.25, 
       fill = c(TRUE, TRUE), fill.alpha = 0.08, 
       xlim = c(-15, 110), ylim = c(40, 110),
       xlab = "Student Achievement Test", 
       ylab = "Peabody Picture Vocabulary Test", 
       label.pos = c(1, rep(NULL, 5), 1))

## High SES students:
heplot(rohwer.ses1, col = c("red", rep("black", 5), "blue"), 
       hypotheses = list("B=0, High SES" = c("n", "s", "ns", "na", "ss")), 
       level = 0.5, cex = 1.25, 
       add = TRUE,   # place both plots on same graphic
       error = TRUE, # error ellipse is not drawn by default with add = TRUE
       fill = c(TRUE, TRUE), fill.alpha = 0.08, 
       xlim = c(-15, 110), ylim = c(40, 110))
