#' ---
#' title: Two-group MANOVA demo
#' ---

# when multivariate analysis pays
# two correlated samples, but with inversely related means

library(MASS)
library(dplyr)
library(ggplot2)
library(ggExtra)

set.seed(1421)
n <- 200
effect <- 1.6
mu1 <- c(-effect/2, -effect/2)
mu2 <- c(effect/2,   effect/2)

sigma <- matrix(c(1, -0.8,
                  -0.8, 1), nrow=2)


dat1 <- as.data.frame(mvrnorm(n=n, mu=mu1, Sigma=sigma))
dat2 <- as.data.frame(mvrnorm(n=n, mu=mu2, Sigma=sigma))

dat <- rbind(cbind(group="A", dat1),
             cbind(group="B", dat2)
            )

sumry <- dat |>
  group_by(group) |>
  summarise(V1m = mean(V1), V2m = mean(V2),
            V1s = sd(V1), V2s = sd(V2))
 

plt <- ggplot(dat, aes(V1, V2, color=group, fill=group)) +
  geom_point() +
  stat_ellipse(geom = "polygon", alpha = 1/10,
               level = 0.9, size=1.5) +
  theme_bw(base_size = 16) +
  theme(legend.position = c(.85, .85))
plt

plt <-
plt + geom_errorbar(data=sumry,
                    aes(V1m, V2m, 
                        ymin=V2m - V2s, 
                        ymax=V2m + V2s), size=1.2, width=.4) +
     geom_errorbarh(data=sumry,
                    aes(V1m, V2m, 
                        xmin=V1m - V1s, 
                        xmax=V1m + V1s), size=1.2, width=.4)

#ggMarginal(plt, type="density")

ggMarginal(plt, type="density", 
           groupColour = TRUE, 
           groupFill = TRUE)


