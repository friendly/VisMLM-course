
library(here)
library(easystats)

load(here("data", "peng.RData"))

mod <- lm(bill_length ~ bill_depth * species, data=peng)

# collection of plots for model checking
check_model(mod)

# the whole shebang, using flexdashboard
model_dashboard(mod)