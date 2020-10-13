## iris data, heplots & candisc examples

library(heplots)
library(candisc)
library(car)

data(iris)
# names(iris)
# names(iris) <- c("SepLen", "SepWid", "PetLen", "PetWid", "Species")

# Exploratory plots

col <- c("red", "blue", "green3")
pch <- 15:17

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

col <- gg_color_hue(3)

## boxplots

op <- par(mfrow=c(1, 4))
for (response in names(iris)[1:4]){
  Boxplot(iris[, response] ~ Species, data=iris,
          ylab=response, axes=FALSE,
          col=col,
          cex.lab = 1.5)
  box()
  axis(2)
  axis(1, at=1:3, labels=c("set.", "vers.", "virg."))
}
par(op)

library(ggplot2)
library(tidyr)

iris_long <- iris %>% 
  gather(Measure, Size, Sepal.Length:Petal.Width) 
 
ggplot(iris_long, aes(x=Species, y=Size, fill=Species)) +
  geom_boxplot() + 
  facet_wrap(. ~ Measure, scales="free_y", nrow=1)


## scatterplot matrix

pairs(iris[1:4],
      pch = 21, 
      bg = c("red", "green3", "blue")[unclass(iris$Species)])


scatterplotMatrix(~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width | Species,
                  data=iris, 
                  smooth=FALSE, regLine=FALSE,
                  ellipse = list(levels=0.68, fill=TRUE, fill.alpha=0.2),
                  by.groups=TRUE,
                  diagonal=FALSE,
                  legend = list(coords="bottomleft", cex=2, title="Species"),
                  col = col,
                  pch = pch,
                  cex = 0.9)



# univariate anova
anova(lm(Sepal.Length ~ Species, data=iris))
anova(lm(Sepal.Width ~ Species, data=iris))
anova(lm(Petal.Lendth ~ Species, data=iris))
anova(lm(Petal.Width ~ Species, data=iris))


for (response in names(iris)[1:4]){
  print( anova( lm (iris[, response]  ~ Species, data=iris)) )
}

# HE plots: testing linear hypotheses for contrasts
C <- matrix(c(0, -1, 1,   
              2, -1, -1), nrow=3, ncol=2)
contrasts(iris$Species) <- C
contrasts(iris$Species)

iris.mod <- lm(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~
               Species, data=iris)

Anova(iris.mod)
coef(iris.mod)

car::scatterplot(Sepal.Width ~ Sepal.Length | Species, data=iris,
                 smooth=FALSE, regLine=FALSE,
                 ellipse = list(levels=0.68, fill=TRUE, fill.alpha=0.2),
                 legend = list(coords="topright", cex=1.2, title="Species"),
                 col = col, pch=15:17, grid=FALSE,
                 xlim=c(2,9), ylim=c(1,5)
                )

heplot(iris.mod, fill=c(T, F))

# add tests for contrasts
hyp <- list("V:V"="Species1","S:VV"="Species2")

heplot(iris.mod, hypotheses=hyp)


# compare with effect-size scaling
heplot(iris.mod, hypotheses=hyp, size="effect", add=TRUE)

# all pairwise plots

pairs(iris.mod, hypotheses=hyp, hyp.labels=FALSE,
      fill=TRUE, fill.alpha=0.1)


# candisc plots

# iris data
iris.can <- candisc(iris.mod, data=iris)
iris.can


#-- assign colors and symbols corresponding to species
#col <- c("red", "brown", "green3")
pch <- 15:17
plot(iris.can, col=col, pch=pch, ellipse=TRUE, scale=6)

plot(iris.can, col=col, pch=15:17, rev.axes = c(TRUE, FALSE),
     ellipse=TRUE, scale=8, var.lwd = 2, var.col = "black")


heplot(iris.can, rev.axes = c(TRUE, FALSE), 
       fill = TRUE, fill.alpha = 0.2,
       scale = 30)

# 1-dim plot
# iris.can1 <- candisc(iris.mod, data=iris, ndim=1)
# plot(iris.can1)

# or
plot(iris.can, which=1, points.1d=TRUE, pch=15:17, rev.axes=TRUE)
plot(iris.can, which=2, points.1d=TRUE)
plot(iris.can, which=3, points.1d=TRUE)





