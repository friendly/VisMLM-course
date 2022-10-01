# testing other biplot functions

library(here)
load(here("data", "peng.RData"))


library(PCAtools)
peng.pca1 <- PCAtools::pca(peng[,3:6], scale=TRUE, transposed = TRUE, metadata = peng$species)
biplot(peng.pca1)

peng.pca <- prcomp (~ bill_length + bill_depth + flipper_length + body_mass,
                    data=peng,
                    na.action=na.omit,
                    scale. = TRUE)

