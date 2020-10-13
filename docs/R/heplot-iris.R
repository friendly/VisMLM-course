data(iris)
library(heplots)

folder <- "C:/Documents and Settings/friendly/My Documents/My Dropbox/2014 - Visualizing MLMs Paper/fig"
setwd(folder)


## iris data
contrasts(iris$Species)<-matrix(c(0,-1,1, 2, -1, -1), 3,2)
contrasts(iris$Species)

iris.mod <- lm(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~
Species, data=iris)

# basic HE plots

# heplot(iris.mod, size="effect", fill=TRUE, fill.alpha=c(0.3, 0.1))


# plot with same x,y limits
op <- par(mar=c(4,4,1,1)+0.1)
res <- heplot(iris.mod, fill=TRUE, fill.alpha=c(0.3, 0.1), cex=1.25, cex.lab=1.5)
label <- expression(paste("Significance scaling:", H / lambda[alpha], df[e]))
text(7.5, 4.5, label, cex=1.5)
par(op)
dev.copy2pdf(file="heplot-iris1a.pdf")

op <- par(mar=c(4,4,1,1)+0.1)
heplot(iris.mod, size="effect", fill=TRUE, fill.alpha=c(0.3, 0.1), cex=1.25, cex.lab=1.5,
	xlim=res$xlim, ylim=res$ylim)
label <- expression(paste("Effect size scaling:", H / df[e]))
text(7.5, 4.5, label, cex=1.5)
par(op)
dev.copy2pdf(file="heplot-iris1b.pdf")

# pairs plot
pairs(iris.mod, fill=TRUE, fill.alpha=c(0.3, 0.1))
dev.copy2pdf(file="heplot-iris2.pdf")


# showing contrasts

hyp <- list("V:V"="Species1","S:VV"="Species2")
heplot(iris.mod, hypotheses=hyp)
# compare with effect-size scaling
heplot(iris.mod, hypotheses=hyp, size="effect", add=TRUE)

# try filled ellipses
heplot(iris.mod, hypotheses=hyp, fill=TRUE, fill.alpha=0.2, col=c("red", "blue"))
heplot(iris.mod, hypotheses=hyp, fill=TRUE, col=c("red", "blue"), lty=c(0,0,1,1))
# vary label position and fill.alpha
heplot(iris.mod, hypotheses=hyp, fill=TRUE, fill.alpha=c(0.3,0.1), col=c("red", "blue"), 
       lty=c(0,0,1,1), label.pos=0:3)

