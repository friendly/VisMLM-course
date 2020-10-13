#' ---
#' title: "Analysis of Neuro Cog data"
#' author: "Michael Friendly, Matthew Sigal"
#' date: "08 Jun 2016"
#' ---

#' This script reproduces all of the analysis and graphs for the MANOVA of the `NeuroCog` data 
#' in the paper and also includes other analyses not described there.  It is set up as an
#' R script that can be "compiled" to HTML, Word, or PDF using `knitr::knit()`.  This is most
#' convenient within R Studio via the `File -> Compile Notebook` option.


#+ echo=FALSE
knitr::opts_chunk$set(warning=FALSE, message=FALSE, R.options=list(digits=4))


#' ## Load packages and the data
library(heplots)
library(candisc)
data(NeuroCog, package="heplots")

#' ## Fit the MANOVA model, test hypotheses
NC.mlm <- lm(cbind( Speed, Attention, Memory, Verbal, Visual, ProbSolv) ~ Dx,
               data=NeuroCog)
Anova(NC.mlm)

#' Test contrasts: Control vs. others (`Dx1`), and Schizo- vs. schizoaffective (`Dx2`).
#' These were set up in the original data frame.
contrasts(NeuroCog$Dx)
print(linearHypothesis(NC.mlm, "Dx1"), SSP=FALSE)
print(linearHypothesis(NC.mlm, "Dx2"), SSP=FALSE)

#' ## Examine residuals in a chi-square QQ plot
#' 
#' The standard plot shows no problems. Using robust estimates to calculate Mahalanobis $D^2$ shows a few
#' potential outliers, but we ignore these.
cqplot(NC.mlm)
cqplot(NC.mlm, method="mve", id.n=4)

#' ## HE plot

heplot(NC.mlm, fill=TRUE, fill.alpha=0.1,
	cex.lab=1.3, cex=1.25)

#' ## pairwise HE plots
#' 
#' All pairs of variables show the same pattern of group differences on the responses
pairs(NC.mlm, var.cex=2, fill=TRUE, fill.alpha=0.1)

#' ## canonical discriminant analysis 
#' 
#' The canonical analysis shows a simple, 1-dimensional structure to group differences on
#' the response variables.  Plotting the canonical variates provides a very simple interpretation.                     
NC.can <- candisc(NC.mlm)
NC.can

#'  tweak `var.pos` of labels in this plot to avoid overlap
pos <- c(4, 1, 4, 4, 1, 3)
col <- c("red", "darkgreen", "blue")
plot(NC.can, ellipse=TRUE, rev.axes=c(TRUE,FALSE), pch=c(7,9,10),
	var.cex=1.2, cex.lab=1.5, var.lwd=2,  scale=4.5, col=col,
	var.col="black", var.pos=pos,
	prefix="Canonical dimension ")

#' ## equivalent plot, as an HE plot
#' 
#' The variable vectors show the relationships (correlations) of the response variables to the
#' canonical dimensions.
heplot(NC.can, 
	fill=TRUE, fill.alpha=.1,  scale=6,
	cex.lab=1.5, var.cex=1.2, 
	var.lwd=2, var.col="black",  var.pos=pos,
	label.pos=c(3,1), cex=1.1, rev.axes=c(TRUE,FALSE),
	prefix="Canonical dimension ")


#' ## fit a robust MLM, downweighting possible outliers
#' 
#' As a sensitivity analysis, we could compare results with those of a robust MLM

NC.rlm <- robmlm(cbind( Speed, Attention, Memory, Verbal, Visual, ProbSolv) ~ Dx,
               data=NeuroCog)
Anova(NC.rlm)

# plot weights
#plot(NC.rlm, id.weight=.5)
