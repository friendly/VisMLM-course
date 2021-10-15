---
  title: "visualizing regression models with ggplot2"
output:
  html_output
---
  
# 

#' # models and plots for gas mileage (mpg)
data(mtcars)
str(mtcars)


library(ggplot2)
library(dplyr)

# ggplot(mtcars, aes(x = hp, y = mpg)) +
#   geom_point(size = 2) +
#   geom_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1.5) +
#   labs(x = "hp (horsepower)", y = "mpg (miles per gallon)",
# 	     title = "lm(mpg ~ hp, data=mtcars)")

ggplot(mtcars, aes(x = disp, y = mpg)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1.5) +
  labs(x = "disp (displacement)", y = "mpg (miles per gallon)",
       title = "lm(mpg ~ disp, data=mtcars)")

#' ## add transmission type as another predictor
#' ## Parallel slopes model
mtcars <- mtcars %>% 
	mutate(am = factor(am)) %>%
	mutate(am = recode(am, "0" = "automatic", "1" = "manual"))
head(mtcars, 5)

mod2 <- lm(mpg ~ disp + am, data = mtcars)

# Use broom::augment to get separare fitted values
library(broom)

ggplot(augment(mod2), 
       aes(x = disp, y = mpg, color = am)) +
  geom_point(size = 2) +
  geom_line(aes(y = .fitted), size = 1.5) +
  labs(x = "disp (displacement)", y = "mpg (miles per gallon)",
       title = "lm(mpg ~ disp + am, data=mtcars)")

# use separate geom_smooths  
ggplot(mtcars, 
       aes(x = disp, y = mpg, color = am)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1.5) +
  labs(x = "disp (displacement)", y = "mpg (miles per gallon)",
       title = "lm(mpg ~ disp , data=mtcars), by am")

#' ## Modelling the interaction

mod3 <- lm(mpg ~ disp * am, data = mtcars)

ggplot(augment(mod3), 
       aes(x = disp, y = mpg, color = am)) +
  geom_point(size = 2) +
  geom_line(aes(y = .fitted), size = 1.5) +
  labs(x = "disp (displacement)", y = "mpg (miles per gallon)",
       title = "lm(mpg ~ disp * am, data=mtcars)")

# Generate text reports from a statistical model with the `report` package

library(report)   # create statistical reports
library(clipr)    # copy to clipboard

report(mod2)
report(mod3)

write_clip(report(mod2))

report_table(mod2)



#' ## Diagnostic plots
#' 
par(mfrow=c(2,2))
plot(mod3)

# easier with ggfortify
library(ggfortify)
autoplot(mod3)


