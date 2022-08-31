---
  #' title: Penguins data: tidy models
---

library(here)
library(broom)
library(effects)
library(ggplot2)

load(here("data", "peng.RData"))

ggplot(data=peng, aes(x=body_mass, y=bill_length, color=species, fill=species)) +
  geom_point() +
  geom_smooth(method = "lm", size=1.5) +
  stat_ellipse(size=1.5) +
  theme_bw(base_size=16) +
  theme(legend.position = c(.8, .2))


#' ## fit model for bill_length
peng.mod0 <- lm(bill_length ~ body_mass + sex + species + island, data=peng)
summary(peng.mod0)

#' ## visualize model effects
#' plot the predicted effects for the model terms
plot(allEffects(peng.mod0))

#' ## A more compact & tidy summary
glance(peng.mod0)

#' tidy() gives the model term statistics
tidy(peng.mod0)

#' ## augment() gives observation-level statistics
augment(peng.mod0) %>% slice(1:5)

#' ## fit multiple models
models <- peng |>
  group_by(sex, island) |>
  do(mod = lm(bill_length ~ body_mass, data = .))




#' ## plot the predicted effects for the model terms
plot(allEffects(peng.mod0))

