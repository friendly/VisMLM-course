suppressPackageStartupMessages({
  library(visreg)
  library(knitr)
  library(tidyverse)
  library(patchwork)
  library(MASSExtra)
#  library(car)
})

# revise theme
theme_set(
		theme_bw() + 
		theme(plot.title = element_text(hjust = 0.5, size=18)) +
		theme(axis.title = element_text(size=16))
		)

# Box-Cox 

cars.mod <- lm(MPG.city ~ Weight, Cars93)
coef(cars.mod)

summary(cars.mod)

#boxcox(cars.mod)  ## MASS
op <- par(lwd=2)
box_cox(cars.mod) ## MASSExtra tweak
par(op)

lambda(cars.mod)


## ---- fig.height=5, fig.width=10, out.width="100%", fig.cap="The Box-Cox transformation effect"----
p0 <- ggplot(Cars93) + aes(x = Weight) + 
		geom_point(colour = "steelblue", size=3)  + 
		xlab("Weight (lbs)") +
  	geom_smooth(se = FALSE, method = "loess", formula = y ~ x, colour = "black")

p1 <- p0 + aes(y = MPG.city) + 
		ylab("Miles per gallon (MPG)") + 
		ggtitle("Untransformed response") 
p1

p2 <- p0 + aes(y = bc(MPG.city, lambda(cars.mod))) + 
		ggtitle("Transformed response") + 
	  ylab(bquote(bc(MPG, .(round(lambda(cars.mod), 2)))))
p2

p1 + p2

## ---- fig.width=8, fig.height=6, fig.align="center", out.width="75%"----------
big_model <- lm(medv ~ . + (rm + tax + lstat + dis)^2 + poly(dis, 2) + poly(rm, 2) +
                   poly(tax, 2) + poly(lstat, 2), Boston)
big_model %>% drop_term(k = "GIC") %>% plot() %>% kable(booktabs=TRUE, digits=3)

## ---- fig.width=8, fig.height=6, fig.align="center", out.width="75%"----------
base_model <- lm(medv ~ ., Boston)
gic_model <- step_GIC(base_model, scope = list(lower = ~1, upper = formula(big_model)))
drop_term(gic_model) %>% plot() %>% kable(booktabs = TRUE, digits = 3)

## ---- fig.width=14, fig.height=6, fig.align="center", out.width="100%", fig.cap="Two component visualisations for the Boston house price model"----
capture.output(suppressWarnings({
   g1 <- visreg(gic_model, "dis", plot = FALSE)
   g2 <- visreg(gic_model, "lstat", plot = FALSE)
   plot(g1, gg = TRUE) + plot(g2, gg = TRUE) 
})) -> void

