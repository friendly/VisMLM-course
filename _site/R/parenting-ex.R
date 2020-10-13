library(ggplot2)
library(dplyr)
library(heplots)

data(Parenting, package="heplots")

ggplot(Parenting, aes(x=caring, y=play, color=group)) +
  geom_point(size=2, 
             aes(shape=group)) +
  stat_ellipse(type="norm", level=0.68, 
               size = 1.2,
               aes(fill = group),
               geom = "polygon", alpha = 1/8) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.85))



mParenting  <- Parenting %>% 
  select(group, caring, play) %>%
  group_by(group) %>%
  mutate(mcaring = mean(caring), mplay = mean(play))

dParenting  <- Parenting %>% 
  select(group, caring, play) %>%
  group_by(group) %>%
  mutate(dcaring = caring - mean(caring), dplay = play - mean(play))

ggplot(dParenting, aes(x=dcaring, y=dplay, color=group)) +
  geom_point(size=2, 
             aes(shape=group)) +
  stat_ellipse(type="norm", level=0.68, 
               size = 1.2,
               aes(fill = group),
               geom = "polygon", alpha = 1/8) +
  theme_bw() +
  theme(legend.position = c(0.85, 0.85))


parenting.mod <- lm(cbind(caring, play, emotion) ~ group, data=Parenting)

Anova(parenting.mod)

heplot(parenting.mod)