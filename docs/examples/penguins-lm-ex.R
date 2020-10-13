#' ---
#' title: "Linear models for penguins data"
#' author: "Michael Friendly"
#' date: "11 Oct 2020"
#' ---

#+ echo=FALSE
knitr::opts_chunk$set(warning=FALSE, message=FALSE, R.options=list(digits=4))

#' ## Load packages
library(dplyr)
library(ggplot2)
library(car)
library(effects)
library(palmerpenguins)

#' ## clean up variable names, get rid of NAs, etc.
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

#' ## scatterplot matrix
#' 
#' `car::scatterplotMatrix` with data ellipses and regression lines by species

#+ fig.height=6, fig.width=6
scatterplotMatrix(~ bill_length + bill_depth + flipper_length + body_mass | species,
                  data=peng,
                  col = ggplotColours(3),
                  ellipse=list(levels=0.68))

#' ## Faceted plot by island
#' 
#+ fig.width=8
ggplot(peng, aes(x=body_mass, y=bill_length, color=sex)) + 
  geom_point() + 
  geom_smooth(method="lm", formula = "y ~ x") +
  facet_grid(~ island) +
  theme_bw()

#' ## look for non-linear relations
#' Fitting a quadratic is one simple way to check for non-linearity

#+ fig.width=8
ggplot(peng, aes(x=body_mass, y=bill_length, color=sex)) + 
  geom_point() + 
  geom_smooth(method="lm", formula = "y ~ poly(x,2)") +
  facet_grid(~ island) +
  theme_bw()

#+ fig.width=8
ggplot(peng, aes(x=body_mass, y=bill_length, color=sex, shape=species)) + 
  geom_point() + 
  geom_smooth(method="lm", formula = "y ~ poly(x,2)") +
  facet_wrap(island ~ species) +
  theme_bw()


#' ## fit a model for bill_length

peng.mod0 <- lm(bill_length ~ body_mass + sex + species + island, data=peng)
Anova(peng.mod0)

#' ## try an interaction term
peng.mod1 <- lm(bill_length ~ body_mass * sex + species + island, data=peng)
Anova(peng.mod1)

#' ## test all interactions
peng.mod2 <- lm(bill_length ~ body_mass * (sex + species + island), data=peng)
Anova(peng.mod2)

#' ## compare models
anova(peng.mod0, peng.mod1, peng.mod2)

#' ## effect plots
peng.eff <- allEffects(peng.mod1)
plot(peng.eff)
