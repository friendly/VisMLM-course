#' ---
#' title: "Simple demo of multivariate linear models"
#' author: "Michael Friendly"
#' date: "8 Mar 2021"
#' ---

# load the data & packages
data(mathscore, package="heplots")
library(car)
library(heplots)

# Fit the MLM
math.mod <- lm(cbind(BM, WP) ~ group, data=mathscore)
Anova(math.mod)

# show scatterplot with data ellipses

cols <- c("darkgreen", "blue", "red")
scatterplot(WP ~ BM | group, data=mathscore, 
            xlab="Basic math", ylab="Word problems",
            ellipse=list(levels=0.68), smooth=FALSE, 
            xlim = c(130, 210),
            pch=c(15,16), col=cols,
            legend=list(coords = "topright"))

# show within-group covariance ellipses & pooled

covEllipses(mathscore[,2:3], mathscore$group, cex=2,
	xlab="Basic math", ylab="Word problems",
	main = "Methods of teaching algebra", 
	fill = c(FALSE, FALSE, TRUE),
	col = cols,
	cex.lab=1.5
	)
# add points
pch <- ifelse(mathscore$group==1, 15, 16)
col <- ifelse(mathscore$group==1, "darkgreen", "blue")
points(mathscore[,2:3], pch=pch, col=col, cex=1.25)



# HE plot

heplot(math.mod, fill=TRUE, cex=2, cex.lab=1.8,
	xlab="Basic math", ylab="Word problems")

# Canonical discriminant plots

library(candisc)
mod.can <- candisc(math.mod)
mod.can

mod.can$structure
mod.can$scores

plot(mod.can, var.lwd=3, points.1d = TRUE, pch=c(15,16), col=cols)




