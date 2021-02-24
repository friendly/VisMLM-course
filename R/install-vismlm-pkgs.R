# script to install needed packages for my VisMLM course


# set CRAN mirror
local({r <- getOption("repos")
       r["CRAN"] <- "https://cloud.r-project.org"
       options(repos=r)
       })

# list of packages to install
pkgs <- c("candisc", "car", "corrgram", "dplyr", "effects", "ggplot2",     
	"heplots", "HH", "knitr", "modelsummary", "stargazer",
	"rgl", "visreg",
	"tidyverse",  ) 

# install the above, along with any dependencies
install.packages(pkgs, dependencies=TRUE)
# update any recently modified packages
update.packages(ask='graphics')

