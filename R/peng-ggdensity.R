# compare 2D kde density estimates to data ellipses

library(here)
library(ggplot2)
library(ggdensity)

load(here("data", "peng.RData"))

ggplot(peng, aes(flipper_length, bill_length, fill = species)) +
  geom_hdr(
    method = "kde",
    probs = c(0.99, 0.95, 0.8, 0.68)
    ) +
  geom_point(shape = 21, size=2.5)

ggplot(peng, aes(flipper_length, bill_length, fill = species)) +
  geom_hdr(
    method="mvnorm", 
    probs = c(0.99, 0.95, 0.8, 0.68)
    ) +
  geom_point(shape = 21, size=2.5)

peng.pca <- prcomp (~ bill_length + bill_depth + flipper_length + body_mass,
                    data=peng,
                    na.action=na.omit,
                    scale. = TRUE)

# try using ggdensity::geom_hdr  -- doesn't work
library(ggbiplot)
ggbiplot(peng.pca, obs.scale = 1, var.scale = 1,
         groups = peng$species, 
         varname.size = 5, 
         ellipse = FALSE, circle = TRUE) +
  geom_hdr(
    aes(fill = species),
    method="mvnorm", 
    probs = c(0.99, 0.95, 0.8, 0.68)
  ) +
  scale_color_discrete(name = 'Penguin Species') +
  theme_minimal() +
  theme(legend.direction = 'horizontal', legend.position = 'top')

