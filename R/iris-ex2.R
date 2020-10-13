library(heplots)
library(candisc)

#' Box's M test 	
iris.boxm <- boxM(iris[, 1:4], iris[, "Species"])
iris.boxm

plot(iris.boxm, gplabel="Species")

op <- par(mfrow=c(2,2), mar=c(5,4,1,1))
plot(iris.boxm, gplabel="Species", which="product")
plot(iris.boxm, gplabel="Species", which="sum")
plot(iris.boxm, gplabel="Species", which="precision")
plot(iris.boxm, gplabel="Species", which="max")
par(op)

eigs <- summary(iris.boxm, quiet=TRUE)$eigs
op <- par(mar=c(5,4,1,1))
matplot(1:4, log(eigs), type="b",
	col = c("red", "green3", "blue", "black"),
	lwd=c(rep(1,3), 3), pch=c('s', 'v', 'g', 'p'),
	lty=c(rep(2,3), 1), 
	cex=1.2, cex.lab=1.25,
	xlab="Dimension", ylab="log Eigenvalue")

legend("topright", 
	col=c("red", "green3", "blue", "black"),
	legend=c("setosa", "versicolor", "virginca", "pooled"),
	pch=c('s', 'v', 'g', 'p')
	)
par(op)

iris.can <- candisc(iris.mod)
iris.can

# multivariate Levene tests

irisdev <- abs( colDevs(iris[,-5], iris$Species, median) )
irisdev <- data.frame( irisdev, Species=iris$Species)
irisdev.mod <-lm( as.matrix(irisdev[,1:4]) ~ Species, data=irisdev)
Anova(irisdev.mod)

pairs(irisdev.mod, fill=TRUE, fill.alpha=.1)

irisdev.can <- candisc(irisdev.mod)
irisdev.can

op <- par(mar=c(5,5,1,2)+.1, cex.lab=1.25)
plot(irisdev.can, ellipse=TRUE, var.lwd=2, scale=4)
par(op)


	