# script to install needed packages for my VisMLM course

# set CRAN mirror
local({r <- getOption("repos")
       r["CRAN"] <- "https://cloud.r-project.org"
       options(repos=r)
       })

# list of packages to install
pkgs <- c("arm", "broom", "candisc", "car", "corrgram", "dplyr", "effects", "ggbiplot", 
          "ggplot2", "glue",  "heplots", "knitr", "learnr", "MASS", "modelsummary", 
          "palmerpenguins", "stargazer", "rgl", "rpart", "rpart.plot",
          "tidyverse", "visreg") 

# install the above, along with any dependencies
install.packages(pkgs, dependencies=TRUE)
# update any recently modified packages
update.packages(ask='graphics')

