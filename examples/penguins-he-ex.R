#' ---
#' title: "Penguins data: Multivariate EDA & HE plots"
#' author: "Michael Friendly"
#' date: "12 Oct 2020"
#' ---

#+ echo=FALSE
knitr::opts_chunk$set(warning=FALSE, message=FALSE, R.options=list(digits=4))

#' ## Load packages & penguins data


library(dplyr)
library(tidyr)
library(car)
library(heplots)
library(candisc)
library(ggplot2)
#library(ggbiplot)

library(palmerpenguins)
data(penguins)

#' Clean up variable names, etc.
peng <- penguins %>%
	rename(
         bill_length = bill_length_mm, 
         bill_depth = bill_depth_mm, 
         flipper_length = flipper_length_mm, 
         body_mass = body_mass_g
         ) %>%
  mutate(species = as.factor(species),
         island = as.factor(island),
         sex = as.factor(substr(sex,1,1))) %>%
  filter(!is.na(bill_depth),
         !is.na(sex))

str(peng)

#' ## Principal component analysis of quantitative measures

#+ peng.pca
peng.pca <- prcomp (~ bill_length + bill_depth + flipper_length + body_mass,
                    data=peng,
                    na.action=na.omit,
                    scale. = TRUE)

peng.pca

#' ### screeplot shows the relative variances of the components.
screeplot(peng.pca, type = "line", lwd=3, cex=3, 
		main="Variances of PCA Components")

#' ## Results best seen in a biplot

#+ ggbiplot
library(ggbiplot)
ggbiplot(peng.pca, obs.scale = 1, var.scale = 1,
         groups = peng$species, 
         varname.size = 5, 
         ellipse = TRUE, circle = TRUE) +
  scale_color_discrete(name = 'Penguin Species') +
  theme_minimal() +
  theme(legend.direction = 'horizontal', legend.position = 'top')


#' ## Scatterplot matrix
#' 
#+ scatmat
scatterplotMatrix(~ bill_length + bill_depth + flipper_length + body_mass | species,
                  data=peng,
                  ellipse=list(levels=0.68),
                  col = scales::hue_pal()(3),
                  legend=list(coords="bottomright"))


#' ## Boxplots

#' ### one response variable

#+ boxplot
ggplot(peng, aes(x=species, y=body_mass, fill=species)) +
  geom_point() +
  geom_boxplot() +
  facet_wrap(. ~ sex + island) +
  theme(legend.direction = 'horizontal', legend.position = 'top') 


#' ### multiple responses

# peng_long <- peng %>% 
#   tidyr::gather(Measure, Size, bill_length:body_mass) 
#  
# ggplot(peng_long, aes(x=species, y=Size, fill=species)) +
#   geom_boxplot() + 
#   facet_wrap(. ~ Measure, scales="free_y", nrow=1)
# 
# ggplot(peng_long, aes(x=species, y=Size, fill=species)) +
#   geom_violin() + 
#   facet_wrap(. ~ Measure, scales="free_y", nrow=1)

#' ## Covariance ellipses

cols = c(scales::hue_pal()(3), "black")
covEllipses(peng[,3:4], peng$species, 
            col=cols, cex =1.5, cex.lab=1.5,
            fill=TRUE, fill.alpha=c(.1, .1, .1, .05))
col <- (scales::hue_pal()(3))[peng$species]
points(peng[,3:4], col=col)

covEllipses(peng[,5:6], peng$species, 
            col=cols, cex =1.5, cex.lab=1.5,
            #            fill=c(rep(FALSE,3), TRUE), fill.alpha=.1)
            fill=TRUE, fill.alpha=c(.1, .05, .05, .05))
col <- (scales::hue_pal()(3))[peng$species]
points(peng[,5:6], col=col)

#' ## MANOVA

#+ manova
contrasts(peng$species)<-matrix(c(1,-1,0, -1, -1, -2), 3,2)
contrasts(peng$species)
peng.mod0 <-lm(cbind(bill_length, bill_depth, flipper_length, body_mass) ~ species, data=peng)
Anova(peng.mod0)


# significance scaling
op <- par(mar=c(4,4,1,1)+0.1)
heplot(peng.mod0, fill=TRUE, fill.alpha=0.1,
       size="evidence")
label <- expression(paste("Significance scaling:", H / lambda[alpha], df[e]))
text(85, 30, label, cex=1.5, pos=2)
par(op)

# effect scaling
op <- par(mar=c(4,4,1,1)+0.1)
heplot(peng.mod0, fill=TRUE, fill.alpha=0.1, 
       size="effect",
       xlim=c(35,52), ylim=c(14,20))
label <- expression(paste("Effect size scaling:", H / df[e]))
text(52, 19.8, label, cex=1.5, pos=2)
par(op)


op <- par(mar=c(4,4,1,1)+0.1)
heplot(peng.mod0, variables=3:4,  
       fill=TRUE, fill.alpha=0.2, size="effect")
par(op)


op <- par(mar=c(4,4,1,1)+0.1)
hyp <- list("A:C"="species1","AC:G"="species2")
heplot(peng.mod0, fill=TRUE, fill.alpha=0.2, hypotheses=hyp, size="effect")
par(op)



heplot(peng.mod0, size="effect", fill=TRUE, fill.alpha=0.2)

pairs(peng.mod0, size="effect", fill=c(TRUE, FALSE), 
      fill.alpha=0.1, lwd=c(1,3))

# library(rgl)
# heplot3d(peng.mod0, shade=TRUE, shade.alpha=0.2, wire=FALSE, size="effect")
# 
# par3d(windowRect = c(0, 0, 500, 500) + 40)
# 
# #play3d(spin3d(axis = c(0, 1, 0), rpm = 6), duration = 20)
# 
# movie3d(spin3d(axis = c(0, 1, 0), rpm = 6), movie="peng3d", dir=".", 
#         duration = 6, fps=5)

### Add other predictors

peng.mod2 <-lm(cbind(bill_length, bill_depth, flipper_length, body_mass) ~ species + sex, 
               data=peng)
Anova(peng.mod2)

hyp <- list("A:C"="species1","AC:G"="species2")
op <- par(mar=c(4,4,1,1)+0.1)
heplot(peng.mod2, size="effect", fill=TRUE, fill.alpha=0.2, hypotheses=hyp)
par(op)

peng.mod3 <-lm(cbind(bill_length, bill_depth, flipper_length, body_mass) ~ species + sex + island, data=peng)
Anova(peng.mod3)
heplot(peng.mod3, size="effect", fill=TRUE, fill.alpha=0.2)


#' ## Canonical discriminant analysis


(peng.can <- candisc(peng.mod0))

#' ## Plot as an HE plot
heplot(peng.can, size="effect", fill=c(TRUE, FALSE),
       scale=3.5,
       lwd=c(1,4), cex=1.25,
       var.lwd=2, var.cex=1.3, cex.lab=1.6)

#' ## Plot as a biplot showing observation scores
plot(peng.can, ellipse = TRUE,
     scale=6,
     var.lwd=2, var.cex=1.3, cex.lab=1.6)
par(op)

