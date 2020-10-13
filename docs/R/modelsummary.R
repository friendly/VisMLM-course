library(modelsummary)
library(kableExtra)
library(gt)

url <- 'https://vincentarelbundock.github.io/Rdatasets/csv/HistData/Guerry.csv'
dat <- read.csv(url)

models <- list()
models[['OLS 1']] <- lm(Donations ~ Literacy + Clergy, data = dat)
models[['Poisson 1']] <- glm(Donations ~ Literacy + Commerce, family = poisson, data = dat)
models[['OLS 2']] <- lm(Crime_pers ~ Literacy + Clergy, data = dat)
models[['Poisson 2']] <- glm(Crime_pers ~ Literacy + Commerce, family = poisson, data = dat)
models[['OLS 3']] <- lm(Crime_prop ~ Literacy + Clergy, data = dat)

msummary(models, statistic = 'statistic')

