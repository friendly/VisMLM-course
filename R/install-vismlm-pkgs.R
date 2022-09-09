# script to install needed packages for my VisMLM course

# set CRAN mirror
local({r <- getOption("repos")
       r["CRAN"] <- "https://cloud.r-project.org"
       options(repos=r)
       })

# list of packages to install
pkgs <- c("arm", "broom", "candisc", "car", "corrgram", "dplyr", "effects", "GGally", 
          "ggbiplot", "ggplot2", "glue",  "heplots", "knitr", "learnr", "MASS", "modelsummary", 
          "MVN", "palmerpenguins", "stargazer", "rgl", "rpart", "rpart.plot", "rstatix",
          "tidyverse", "visreg") 

# install the above, along with any dependencies
install.packages(pkgs, dependencies=TRUE)
# update any recently modified packages
update.packages(ask='graphics')

