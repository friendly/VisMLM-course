#' ---
#' title: "Linear models for penguins data"
#' author: "Michael Friendly"
#' date: "11 Feb 2021"
#' output:
#'   html_document:
#'     theme: readable
#'     code_download: true
#' ---

#+ echo=FALSE
knitr::opts_chunk$set(warning=FALSE, message=FALSE, R.options=list(digits=4))

#' ## Load packages
library(dplyr)
library(ggplot2)
library(car)
library(effects)
library(palmerpenguins)

#' ## Clean up variable names, get rid of NAs, etc.
#' 
#' Remove units from variable names (for display purposes). Character variables should be factors.
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

#' ## Function to reproduce ggplot2 default colors
ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

#' ## Scatterplot matrix
#' 
#' `car::scatterplotMatrix` with data ellipses and regression lines by species

#+ fig.height=6, fig.width=6
scatterplotMatrix(~ bill_length + bill_depth + flipper_length + body_mass | species,
                  data=peng,
                  col = ggplotColours(3),
                  ellipse=list(levels=0.68))

#' ## Faceted plot by island
#' 
#' Assume our main interest is in understanding how `bill_length` is related to other variables.
#' We can easily include categorical predictors (`sex` and `island`) using color and faceting.
#' 
#+ fig.width=8, fig.height=4
ggplot(peng, aes(x=body_mass, y=bill_length, color=sex)) + 
  geom_point() + 
  geom_smooth(method="lm", formula = "y ~ x") +
  facet_grid(~ island) +
  theme_bw()

#' ## Quick checks for non-linear relations
#' Fitting a quadratic with `geom_smooth()` is one simple way to check for non-linearity.
#' Another is to use a `method="loess"` smoother.

#+ fig.width=8, fig.height=4
ggplot(peng, aes(x=body_mass, y=bill_length, color=sex)) + 
  geom_point() + 
  geom_smooth(method="lm", formula = "y ~ poly(x,2)") +
  facet_grid(~ island) +
  theme_bw()

#' Do the same, but facet by island & species
#+ fig.width=8
ggplot(peng, aes(x=body_mass, y=bill_length, color=sex, shape=species)) + 
  geom_point() + 
  geom_smooth(method="lm", formula = "y ~ poly(x,2)") +
  facet_wrap(island ~ species) +
  theme_bw()


#' ## Fit a model for `bill_length`

#' Here, use `body_mass` as main quantitative predictor, but control for `sex`, `species` and `island`
peng.mod0 <- lm(bill_length ~ body_mass + sex + species + island, data=peng)
Anova(peng.mod0)

#' ### Try an interaction term
peng.mod1 <- lm(bill_length ~ body_mass * sex + species + island, data=peng)
Anova(peng.mod1)

#' ### Test all interactions
#' 
#' This is a simple screening test to check whether any interactions with `body_mass` are important.
peng.mod2 <- lm(bill_length ~ body_mass * (sex + species + island), data=peng)
Anova(peng.mod2)

#' ### Compare models
#' 
#' `Anova()` tests the additional contribution of each model over the previous one.
anova(peng.mod0, peng.mod1, peng.mod2)

#' ## Effect plots
#' 
#' By default, this plots the mean response (`bill_length`) all the high-order terms in the model
#' (`species`, `island`, and `body_mass * sex`),  averaged over all other variables not shown in a given
#' plot.
#' 
#+ fig.width=8, fig.height=8
peng.eff <- allEffects(peng.mod1)
plot(peng.eff)
