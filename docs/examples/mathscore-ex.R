#' ---
#' title: "Math scores: HE plot examples"
#' author: "Michael Friendly"
#' date: "`r format(Sys.Date())`"
#' output:
#'   html_document:
#'     theme: readable
#'     code_download: true
#' ---

#+ echo=FALSE
knitr::opts_chunk$set(warning=FALSE, message=FALSE, 
             R.options=list(digits=4),
             fig.width=5, fig.height=5)


#' ## Load packages and the data
library(car)
library(heplots)
library(candisc)
library(MASS)       # for lda()
data(mathscore, package="heplots")

#' ## Fit the MLM
math.mod <- lm(cbind(BM, WP) ~ group, data=mathscore)
Anova(math.mod)

#' ## Compare with univariate ANOVAs
#'
#' Univariate tests treat the BM and WP responses as independent &
#' don't take correlation into account.
Anova(lm(BM ~ group, data=mathscore))
Anova(lm(WP ~ group, data=mathscore))

#' ## HE plot

#' `heplot()` returns an object containing useful information about the plot.
#' I save this here, to be able to use the plot limits later so the scales
#' align.
hep <- heplot(math.mod, fill=TRUE, cex=2, cex.lab=1.8,
       xlab="Basic math", ylab="Word problems")

#' ## Multivariate tests: **H** & **E** matrices
math.aov <- Anova(math.mod)

#' Extract the H & E matrices from the object
(H <- math.aov$SSP)

(E <- math.aov$SSPE)

#' ## Compare with direct calculation
resids <- residuals(math.mod)
crossprod(resids)            # E matrix

fit <- fitted(math.mod)
ybar <- colMeans(mathscore[,2:3])
n <- nrow(mathscore)
crossprod(fit) - n * outer(ybar, ybar)   # H matrix


#' ## Visualize data ellipses

#' Set colors and point symbols
cols <- c("darkgreen", "blue", "red")
pch <- ifelse(mathscore$group==1, 15, 16)

#' The **E** matrix is the data ellipse of the residuals -- the pooled within group covariance matrix
covEllipses(resids, mathscore$group, 
            pooled=TRUE, cex=2,
            xlab="Basic math", ylab="Word problems",
            fill = c(F, F, TRUE), fill.alpha=0.1,
            col = cols,
            asp=1,
            cex.lab=1.5)
points(resids, pch=pch, col=cols, cex=1.25)

#' fitted values: **H** matrix
covEllipses(fit, mathscore$group, 
            pooled=FALSE, cex=2,
            xlab="Basic math", ylab="Word problems",
            fill = c(F, F, TRUE), fill.alpha=0.1,
            col = cols,
            asp=1,
            xlim = hep$xlim, ylim = hep$ylim,
            cex.lab=1.5)
points(jitter(fit), pch=pch, col=cols, cex=1.25)
car::dataEllipse(fit[,1], fit[,2], add=TRUE, level=0.68, col="black", lwd=3)


#' ## Discriminant analysis


mod.lda <- MASS::lda(group ~ ., mathscore)

mod.lda
names(mod.lda)

#' ## Run canonical discriminant analysis
mod.can <- candisc(math.mod)
mod.can

#' Show the canonical scores and structure coefficients
mod.can$structure
mod.can$scores

#' Plot the canonical scores and structure coefficients
plot(mod.can, var.lwd=3, points.1d = TRUE, pch=c(15,16), col=cols)

