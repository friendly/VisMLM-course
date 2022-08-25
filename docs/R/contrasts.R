# contrasts

library(faux) # simulated data sets


# three groups

df <- sim_design(between = list(group = c("Asst", "Assoc", "Full")), 
                 n = 50, 
                 mu = c(40, 60, 90),
                 dv = "salary")

str(df)

library(tibble)
df <- tibble(group = c("Asst", "Assoc", "Full"),
             salary = c(40, 60, 90))
df


cmat <- cbind("c1:tenured?" = c(-2, 1, 1),   # tenured vs. not
              "c2:full?"    = c(0, -1, 1))   # full prof vs. assoc

rownames(cmat) <- c("Asst", "Assoc", "Full")
cmat

# four groups, 2 x 2

heaters <- tibble(
  brand  = LETTERS[1:4],
  method = c("radiation", "radiation", "convection", "convection"),
  type   = c("fan", "bar", "fan", "bar")
) 
heaters

cmat <- cbind("c1:rad-conv" = c(1,  1, -1, -1 ),
              "c2:fan-bar"  = c(1, -1,  1, -1),
              "c3:A-B"      = c(1, -1,  0,  0),
              "c4:C-D"      = c(0,  0,  1, -1)
)
rownames(cmat) <- LETTERS[1:4]
cmat

# 4 groups

crops <- tibble(
  crop   = c("lupin", "mustard", "barley", "oats"),
  legume = c("yes", "no", "no", "no"),
  cereal = c(NA, "no", "yes", "yes")
)
crops

cmat <- cbind("c1:legume?" = c(-3, 1, 1, 1),
              "c2:cereal?" = c( 0, -2, 1, 1),
              "c3:oats-barley" = c(0, 0, -1, 1))
rownames(cmat) <- crops$crop
cmat

#rbind(crops, cmat)


