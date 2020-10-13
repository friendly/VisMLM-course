library(candisc)
X <- as.matrix(Rohwer2[, 6:10]) # X variables for High SES students
Y <- as.matrix(Rohwer2[, 3:5])  # Y variables for High SES students
cc <- cancor(X, Y, set.names=c("PA", "Ability"))
cc


X <- Rohwer2[, 6:10] # X variables for High SES students
Y <- Rohwer2[, 3:5]  # Y variables for High SES students
cc <- cancor(X, Y, set.names=c("PA", "Ability"))
cc

cancor(cbind(SAT + PPVT + Raven) ~ n + s + ns + na + ss, data = Rohwer2)

cc <- cancor(cbind(SAT, PPVT, Raven) ~  n + s + ns + na + ss, data=Rohwer2, 
             set.names=c("PA", "Ability"))


png(filename="cancor1.png")
op <- par(mar=c(4,4,1,1)+.1)
cols <-  c("red", "blue", "black", "darkgreen", "darkcyan", 
           "magenta", "gray20")

heplot(cc, hypotheses=list("na+ns"=c("na", "ns")),
       fill = TRUE, fill.alpha=0.1, col=cols,
       label.pos = c(3, rep(1,5), .1), 
       cex=1.4, var.cex=1.25, var.lwd=3, var.col="black")
par(op)
dev.off()


png(filename="cancor2.png")
op <- par(mar=c(4,4,1,1)+.1)
hyp <- list("na+ns"=c("na", "ns"),
            "Regr"=c("n", "s", "ns", "na", "ss"))
heplot(cc, hypotheses=hyp,
       fill = TRUE, fill.alpha=0.1, col=cols,
       label.pos = c(3, rep(1,5), .1),
       cex=1.4, var.cex=1.25, var.lwd=3, var.col="black")

par(op)
dev.off()
