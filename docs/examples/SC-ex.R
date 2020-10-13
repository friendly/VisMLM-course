#' ---
#' title: "Analysis of Social Cognition data"
#' author: "Michael Friendly, Matthew Sigal"
#' date: "08 Jun 2016"
#' ---

#' This script reproduces all of the analysis and graphs for the MANOVA of the `SocialCog` data 
#' in the paper and also includes other analyses not described there.  It is set up as an
#' R script that can be "compiled" to HTML, Word, or PDF using `knitr::knit()`.  This is most
#' convenient within R Studio via the `File -> Compile Notebook` option.

#+ setup, echo=FALSE
knitr::opts_chunk$set(warning=FALSE, message=FALSE, R.options=list(digits=4))

#' ## Load packages and the data
library(heplots)
library(candisc)
library(mvinfluence)
data(SocialCog, package="heplots")
str(SocialCog)

#' ## Fit the MANOVA model, test hypotheses
SC.mlm <-  lm(cbind(MgeEmotions,ToM, ExtBias, PersBias) ~ Dx,
               data=SocialCog)

Anova(SC.mlm)

#' Test contrasts of substantive interest: `Dx1`: Control group vs. schizo groups; `Dx2`: Schizophrenia group
#' vs. schizoaffective.
print(linearHypothesis(SC.mlm, "Dx1"), SSP=FALSE)
print(linearHypothesis(SC.mlm, "Dx2"), SSP=FALSE)

#' ## HE plots

#' This plot shows the first two response variables.  We can also embed the **H** ellipses for the contrasts
#' to see how they relate to differences in the group means.
heplot(SC.mlm, hypotheses=list("Dx1"="Dx1", "Dx2"="Dx2"),
	fill=TRUE, fill.alpha=.1,
	cex.lab=1.5, cex=1.3)


	
#' The `pairs()` plot shows all pairwise views

pairs(SC.mlm, fill=c(TRUE,FALSE), fill.alpha=.1,
	hypotheses=list("Dx1"="Dx1", "Dx2"="Dx2")) 

#' ## Canonical discriminant analysis

#' How do the groups differ in the canonical space accounting for the greatest group separation?
#' 
SC.can <- candisc(SC.mlm)
SC.can

#' The plot shows the canonical scores with data ellipses and variable vectors
#' indicating how they relate to the canonical dimensions. There is a lot of overlap, but the groups are
#' clearly separated along both canonical dimensions.

col <- c("red", "darkgreen", "blue")
plot(SC.can, ellipse=TRUE, pch=c(7,9,10),
	var.cex=1.2, cex.lab=1.5, var.lwd=2,  col=col,
	var.col="black", # var.pos=pos,
	prefix="Canonical dimension ")


#' An equivalent plot, as an HE plot.  We can also project the contrasts among groups into this space.
heplot(SC.can, hypotheses=list("Dx1"="Dx1", "Dx2"="Dx2"),
	fill=TRUE, fill.alpha=.1, 
	var.lwd=2, var.col="black", 
	label.pos=c(3,1), 
	cex=1.25, cex.lab=1.2,
	prefix="Canonical dimension ")

#' ## Examine model residuals in a chi-square QQ plot
cqplot(SC.mlm, id.n=1, main="", cex.lab=1.25)

#' Robust version, using MVE shows 4 possible outliers!
cqplot(SC.mlm, method="mve", id.n=4, main="", cex.lab=1.25)


#' One extreme outlier spoils the pie!  The influence plot shows how it stands out.
influencePlot(SC.mlm, scale=20, fill.alpha.max=.4, cex.lab=1.5, id.cex=1.5)
 
op <- par(mar=c(5,4,1,1)+.1)
influencePlot(SC.mlm, type="LR", scale=20, fill.alpha.max=.4, id.cex=1.5)
par(op)
#dev.copy2pdf(file="SC-LRplot.pdf")



#' ## Refit the model, deleting that case
SC.mlm1 <- update(SC.mlm, subset=rownames(SocialCog)!="15")
SC.mlm1
Anova(SC.mlm1)

print(linearHypothesis(SC.mlm1, "Dx1"), SSP=FALSE)
print(linearHypothesis(SC.mlm1, "Dx2"), SSP=FALSE)

#' ## HE plots of revised model

heplot(SC.mlm1, hypotheses=list("Dx1"="Dx1", "Dx2"="Dx2"),
	fill=TRUE, fill.alpha=.1,
	cex.lab=1.5, cex=1.2)

	
pairs(SC.mlm1, fill=c(TRUE,FALSE), fill.alpha=.1) 

#' ## Canonical discriminant analysis

SC.can1 <- candisc(SC.mlm1)
SC.can1

#' the plot shows the canonical scores with data ellipses and variable vectors
#' indicating how they relate to the canonical dimensions. 
plot(SC.can1, ellipse=TRUE)

#' equivalent plot, as an HE plot

heplot(SC.can1, 
	fill=TRUE, fill.alpha=.1,
	hypotheses=list("Dx1"="Dx1", "Dx2"="Dx2"),
	lwd = c(1, 2, 3, 3),
	col=c("red", "blue", "darkgreen", "darkgreen"),
	var.lwd=2, var.col="black", 
	label.pos=c(3,1), var.cex=1.2, 
	cex=1.25, cex.lab=1.2, scale=2.8,
	prefix="Canonical dimension ")

#' ## Robust MLM

#' As an alternative to selective screening to omit possibly influential cases, we can use an
#' iterative robust method that downweights cases based on the squared Mahalanobis distances
#' of the residuals.
#' 
SC.rlm <- robmlm(cbind( MgeEmotions, ToM, ExtBias, PersBias) ~ Dx,
               data=SocialCog,
               subset=rownames(SocialCog)!="15")

# apply this to all the observations
SC.rlm <- robmlm(cbind( MgeEmotions, ToM, ExtBias, PersBias) ~ Dx,
               data=SocialCog
#               subset=rownames(SocialCog)!="15"
               )

Anova(SC.rlm)
print(linearHypothesis(SC.rlm, "Dx1"), SSP=FALSE)
print(linearHypothesis(SC.rlm, "Dx2"), SSP=FALSE)

pairs(SC.rlm, fill=c(TRUE,FALSE), fill.alpha=.1) 

#' ## Plot the weights from the robust MLM
#' 
#' NB:  There is now a `plot.robmlm()` function in `heplots` that does most of this
Dx <- SocialCog$Dx
bad <- SC.rlm$weights < .6              # points to label
col <- c("red", "darkgreen", "blue")    # colors for groups

plot(SC.rlm$weights, 
	xlab="Case index", 
	ylab="Weight in robust MANOVA", 
	pch=(15:17)[Dx],
	col=col[Dx], ylim=c(0,1), 
	cex = ifelse(bad, 1.4, .9),
	cex.lab = 1.25)

breaks <-cumsum(table(SocialCog$Dx))
ctr <- c(0,breaks[1:2]) + diff(c(0, breaks))/2
abline(v=breaks[1:2]+.5, col="grey")
text(ctr, 0.05+c(0, -.04, 0), levels(Dx), cex=1.2)
# label "outliers"
text((1:nrow(SocialCog))[bad], SC.rlm$weights[bad], 
     labels=rownames(SocialCog)[bad], pos=c(2,4,4))



