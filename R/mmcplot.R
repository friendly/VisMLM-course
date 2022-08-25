library(HH)
library(ggplot2)

data(catalystm)

ggplot(data=catalystm, aes(x=catalyst, y=concent)) +
	geom_boxplot(fill="lightblue", alpha=0.5) +
  geom_point() +
  ylab("Concentration") +
	theme_bw(base_size=16)
	
	
catalystm.aov <- aov(concent ~ catalyst, data=catalystm)
summary(catalystm.aov)

glht(catalystm.aov, linfct = mcp(catalyst = "Tukey"))

catalystm.mmc <-
   mmc(catalystm.aov, linfct = mcp(catalyst = "Tukey"))

options(digits=3)
catalystm.mmc

mmcplot(catalystm.mmc, lwd=c(1,3))

# can we use this with lm?
catalystm.lm <- lm(concent ~ catalyst, data=catalystm)
catalystm.mmc <-
  mmc(catalystm.lm, linfct = mcp(catalyst = "Tukey"))
mmcplot(catalystm.mmc, lwd=c(1,3))


# try lsmeans -> emmeans

library(lsmeans)
library(emmeans)
catalyst.lm <- lm(concent ~ catalyst, data=catalystm)

lsmeans::lsmeans(catalyst.lm, pairwise~catalyst)

emmeans(catalyst.lm, ~ catalyst, adjust="tukey")

