#' ---
#' title: 'Exercise: Visualizing Multivariate Data'
#' author: "Michael Friendly"
#' date: "1/20/2021"
#' output: 
#'   html_document:
#'     code_folding: "hide"
#' ---
#' 

#' 
#' # Introduction
#' 
#' The purpose of this exercise is to introduce you to some basic graphical displays
#' useful for multivariate, particularly those associated with simple one-way
#' MANOVA designs.
#' 
#' ## Penguins data
#' 
#' 


library(palmerpenguins)

#' ## Clean up variable names, get rid of NAs, etc.
#' 
#' Remove units from variable names (for display purposes). Character variables should be factors.
peng <- penguins %>%
  rename(
    bill_length = bill_length_mm, 
    bill_depth = bill_depth_mm, 
    flipper_length = flipper_length_mm, 
    body_mass = body_mass_g
  ) %>%
  mutate(species = as.factor(species),
         island = as.factor(island),
         sex = as.factor(substr(sex,1,1))) %>%
  filter(!is.na(bill_depth),
         !is.na(sex))

