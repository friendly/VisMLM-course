#' ---
#' title: Penguins data: Simpson's paradox
#' ---

#' ## Load packages
library(here)        # A Simpler Way to Find Your Files
library(ggplot2)     # Create Elegant Data Visualisations Using the Grammar of Graphics
library(patchwork)   # The Composer of Plots
library(plotly)      # Create Interactive Web Graphics via 'plotly.js'
library(htmlwidgets) # HTML Widgets for R

#' Simpson’s Paradox is the phenomenon in which a trend observed between variables 
#' is reversed when data are pooled, omitting a meaningful variable.
#' This example examines the relation between bill length and bill depth in the
#' penguins data.
#' 
#' As will be seen, When penguin species is omitted, bill length and depth appear 
#' negatively correlated overall. The trend is reversed when species is included,
#' revealing an obviously positive correlation between bill length and bill depth within species.
#' 
#' The reason for the reversal is that the means of the three groups are negatively
#' correlated, inducing an overall negative relationship.

load(here("data", "peng.RData"))

ggplot2::theme_set(theme_bw(base_size = 16))

#' Ignoring species
p1 <- ggplot(peng, aes(x=bill_length, y=bill_depth)) +
  geom_point(size=2) +
  geom_smooth(method="lm", formula=y~x, linewidth=1.5)

#' Including species
p2 <- ggplot(peng, aes(x=bill_length, y=bill_depth, 
                       color=species, fill=species)) +
  geom_point(size=2) +
  geom_smooth(method="lm", formula=y~x, linewidth=1.5) +
  theme(legend.position = c(.2, .2))

#' Print side-by-side
p1+p2

#' ## Making the graphs interactive using `plotly`

ggplotly(p1)
ggplotly(p2)

#' Print side-by-side
subplot(ggplotly(p1), ggplotly(p2))

#' save as html
saveWidget(ggplotly(p1), file = here("fig", "penguins", "simpsons1.html"))

