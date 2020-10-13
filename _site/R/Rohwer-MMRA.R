# Rohwer data, MMRA

data("Rohwer", package="heplots")
Rohwer2 <- subset(Rohwer, subset=group==2)
rownames(Rohwer2)<- 1:nrow(Rohwer2)

rohwer.mlm <- lm(cbind(SAT, PPVT, Raven) ~ n + s + ns + na + ss, data=Rohwer2)

Anova(rohwer.mlm)

print(linearHypothesis(rohwer.mlm, 
                       c("n", "s", "ns", "na", "ss")), SSP=FALSE)

# how to generate a table of univariate tests?
rohwer.coef <- coef(rohwer.mlm)[-1,]

library(broom)
tidy(rohwer.mod)

rohwer.mod1 <- lm ( SAT   ~ n + s + ns + na + ss, data = Rohwer2)
rohwer.mod2 <- lm ( PPVT  ~ n + s + ns + na + ss, data = Rohwer2)
rohwer.mod3 <- lm ( Raven ~ n + s + ns + na + ss, data = Rohwer2)

library(stargazer)

stargazer(rohwer.mod1, rohwer.mod2, rohwer.mod3,
#	out="./tab/rohwer-mods.tex",
#	nobs=FALSE, 
	type = "text",
	digits=2,
	omit.table.layout = "#",
	star.cutoffs = c(0.05, 0.01, 0.001),
	omit = "Constant",
	table.placement = "!htb",
	label="tab:rohwer-mods",
	title="Univariate regression models for Rohwer data")


cols <-  c("red", "blue", "black", "darkgreen", "darkcyan", "magenta", 
            "gray20")

# Visualize the MMRA model
#heplot(rohwer.mlm, fill=TRUE, fill.alpha=0.1,
#	cex=1.5, cex.lab=1.3, col=cols,
#	lwd=c(1,3))

# Add ellipse to test all 5 regressors
op <- par(mar=c(4,5,1,1)+.1)
hyp <- list("Regr" = c("n", "s", "ns", "na", "ss"))
heplot(rohwer.mlm, 
	hypotheses = hyp,
	fill=TRUE, fill.alpha=0.1, cex=1.5, cex.lab=1.4, col=cols,
	lwd=c(1,3))
par(op)
dev.copy2pdf(file="rohwer-mmra1.pdf")


# View all pairs
pairs(rohwer.mlm, hypotheses = hyp, col=cols, var.cex=4, # lty=c(0,1),
	fill=TRUE, fill.alpha=0.1, cex=1.5)

dev.copy2pdf(file="rohwer-pairs.pdf")

# coefficient plots
coefplot(rohwer.mlm, lwd=2, fill=(1:5) %in% 3:4, 
	main="Bivariate 95% coefficient plot for SAT and PPVT", level=0.95)


X <- as.matrix(Rohwer[1:37, 6:10])
Y <- as.matrix(Rohwer[1:37, 3:5])
cc <- cancor(X, Y, set.names=c("PA", "Ability"))

# note relationship of joint hypothesis to individual ones
heplot(cc, hypotheses=list("na+ns"=c("na", "ns")),
	fill = TRUE, fill.alpha=0.1, 
	label.pos = c(3, rep(1,5), .1),
	cex=1.4, var.cex=1.25, var.lwd=3, var.col="black")
