install.packages("easystats")
library(easystats)

mod <- lm(Sepal.Length ~ Sepal.Width * Species, data=iris)

# collection of plots for model checking
check_model(mod)

# the whole shebang, using flexdashboard
model_dashboard(mod)