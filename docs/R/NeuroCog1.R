library(here)
library(heplots)
library(car)
library(tidyr)
library(dplyr)
library(broom)
library(broom.helpers)
library(purrr)
library(ggplot2)

data(NeuroCog, package="heplots")
str(NeuroCog)

#' Reshape from wide to long
NC_long <- NeuroCog |>
  dplyr::select(-SocialCog, -Age, -Sex) |>
  tidyr::gather(key = response, value = "value", Speed:ProbSolv)

NC_long |>
  group_by(Dx) |>
  sample_n(4) |> ungroup()

#' Boxplots for each variable
ggplot(NC_long, aes(x=Dx, y=value, fill=Dx)) +
  geom_jitter(shape=1, size=0.8, width=0.2) +
  geom_boxplot(width=0.5,  alpha=0.4, outlier.alpha=1, outlier.size = 3, outlier.color = "red") +
  facet_wrap(~response, scales = "free_y", as.table = FALSE) +
  theme_bw() +
  theme(legend.position="bottom",
        axis.title = element_text(size = rel(1.2)),
        axis.text  = element_text(face = "bold"),
        strip.text = element_text(size = rel(1.2)))

# simplified version for slide
ggplot(NC_long, aes(x=Dx, y=value, fill=Dx)) +
  geom_jitter() +
  geom_boxplot() +
  facet_wrap(~response, scales = "free_y") +
  theme_bw() 
  

NC.mlm <- lm(cbind( Speed, Attention, Memory, Verbal, Visual, ProbSolv) ~ Dx,
             data=NeuroCog)
car::Anova(NC.mlm)


# broom::tidy  Extract univariate tests of contrasts from mlm object
tidy(NC.mlm) |>
  mutate(response = factor(response, levels=unique(response))) |>    # keep variable order
  filter(term != "(Intercept)") |>
  arrange(response) |>
  mutate(signif = noquote(gtools::stars.pval(p.value))) |>
  mutate(p.value = noquote(scales::pvalue(p.value))) 

# Same, the long way
NC_long |>
  mutate(response = factor(response, levels=unique(response))) |>    # keep variable order
  group_by(response) |>
  do(tidy(lm(value ~ Dx, .))) |>
  filter(term != "(Intercept)") |>
  mutate(signif = noquote(gtools::stars.pval(p.value))) |>
  mutate(p.value = noquote(scales::pvalue(p.value))) 


# get all F-tests?
aov1 <- anova(lm(Speed ~ Dx, data=NeuroCog))
aov1
tidy(aov1)

# digits has no effect
tidy(aov1, digits=7)

# results are already rounded in the tidy result
res <- tidy(aov1)



NC_long |>
  mutate(response = factor(response, levels=unique(response))) |>    # keep variable order
  group_by(response) |>
  do(tidy(anova(lm(value ~ Dx, .)))) |>
  filter(term != "Residuals") |>
  select(-term) |>
  rename(F = statistic, df1 = df,
         SS = sumsq, MS =meansq) |>
  mutate(df2 = 239) |>                   # kludge: extract dfe from object?    
  relocate(df2, .after = df1) |>
  mutate(signif = noquote(gtools::stars.pval(p.value))) |>
  mutate(p.value = noquote(scales::pvalue(p.value))) 
  
# try this using purrr::map??

NeuroCog |>
  dplyr::select(-SocialCog, -Age, -Sex, -Dx) |>
  purrr::map(~lm(.x ~ NeuroCog$Dx, data = NeuroCog)) |>
  purrr::map(broom::glance) |>
  purrr::keep()
  (tidyr::extract(c("statistic", "df", "p.value"))) #, df, df.residual, sigma, r.squared))

NC_long |>
  nest(response) |>
  fit <- lm(response ~ Dx, data=.) |>
  tidy(fit) |>
  unnest()

  



