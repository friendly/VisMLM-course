scatterplotMatrix(~ prestige + education + income + type ,
                  data=Duncan,
                  regLine = list(method=lm, lty=1, lwd=2, col="black"),
                  smooth=list(smoother=loessLine, spread=FALSE,
                              lty.smooth=1, lwd.smooth=3, col.smooth="red"),
                  ellipse=list(levels=0.68, fill.alpha=0.1))

scatterplotMatrix(~ prestige + education + income, data=Duncan,
                  id = list(n=2),
                  regLine = list(method=lm, lty=1, lwd=2, col="black"),
                  smooth=list(smoother=loessLine, spread=FALSE,
                              lty.smooth=1, lwd.smooth=3, col.smooth="red"),
                  ellipse=list(levels=0.68, fill.alpha=0.1))

scatterplotMatrix(~ prestige + education + income, data=Duncan,
                  id = list(n=2),
                  regLine = list(method=lm, lty=1, lwd=2, col="black"),
                  smooth = FALSE,
                  ellipse=list(levels=0.68, fill.alpha=0.1))


scatterplotMatrix(~ prestige + education + income | type ,
                  data=Duncan, smooth=FALSE)