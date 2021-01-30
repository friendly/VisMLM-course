#' ---
#' title: "visreg plots: air quality data"
#' author: "Michael Friendly"
#' date: "27  Dec 2020"
#' ---

#+ echo=FALSE
library(visreg)
knitr::opts_knit$set(aliases=c(h = 'fig.height', w = 'fig.width'))

#' airquality data: Daily air quality measurements in New York, May to September 1973
#' How does atmospheric ozone depend on other variables?

fit1 <- lm(Ozone ~ Solar.R + Wind + Temp, data=airquality)
summary(fit1)$coef

## ---- h=5, w=5----------------------------------------------------------------
visreg(fit1, "Wind", points=list(cex=1))

# all effects
op <- par(mfrow=c(1,3), cex.lab=1.5)
visreg(fit1, "Solar.R", points=list(cex=1))
visreg(fit1, "Wind", points=list(cex=1))
visreg(fit1, "Temp", points=list(cex=1))
par(op)


#' ## Nonlinear terms

#' Suppose we allow the effect of wind on ozone to be nonlinear by introducing a quadratic term into the model:

op <- par(mar=c(4,4,1,1)+.1)
fit <- lm(Ozone ~ Solar.R + poly(Wind,2) + Temp, data=airquality)
visreg(fit, "Wind", points=list(cex=1))
par(op)

#' Or, a quadratic effect of Temp
op <- par(mar=c(4,4,1,1)+.1)
fit <- lm(Ozone ~ Solar.R + Wind + poly(Temp,2), data=airquality)
visreg(fit, "Temp", points=list(cex=1))
par(op)



#' ## Factor variables & interactions
## -----------------------------------------------------------------------------
airquality$Heat <- cut(airquality$Temp, 3, labels=c("Cool","Mild","Hot"))
fit2 <- lm(Ozone ~ Solar.R + Wind*Heat, data=airquality)

## ---- h=4, w=9, out.width='100%'----------------------------------------------
visreg(fit2, "Wind", by="Heat", layout=c(3,1), points=list(cex=1))

#' ## Overlayed `by=` plots  
visreg(fit2, "Wind", by="Heat", overlay=TRUE, points=list(cex=1))

#' ## Use ggplot engine
visreg(fit2, "Wind", by="Heat", overlay=TRUE, gg=TRUE, points=list(size=2)) + theme_bw()


### ---- h=5, w=6----------------------------------------------------------------
#fit <- lm(Ozone ~ poly(Wind, 2)*poly(Temp, 2), data=airquality)
#visreg2d(fit, "Wind", "Temp")
#

#' ## contour plots using visreg2d

#' Fit quadratics in both Wind & Temp, no interaction
fitp <- lm(Ozone ~ Solar.R + poly(Wind,2) + poly(Temp,2), data=airquality)
summary(fitp)

fitp <- lm(Ozone ~ Solar.R + poly(Wind,2) * poly(Temp,2), data=airquality)
summary(fitp)


visreg2d(fitp, "Wind", "Temp", plot.type="gg") + 
	geom_point(data=airquality, aes(x=Wind, y=Temp)) +
	geom_contour(aes(z=z), color="black") 

#' ## Perspective
visreg2d(fitp, "Wind", "Temp", plot.type="persp")



