#### Demo: Fit 3PL, DINA, and DINO models ####
## 2023-02-27; Joseph N. DeWeese
# 
# use Bonifay & Cai + Niels Waller code to generate data
# fit 3pl with mirt, fit DINO and DINA with GDINA
# I also ended up fitting the EIFA and bifactor models in mirt


# 0. setup ####
#install.packages("mirt")
library(mirt)
#install.packages("GDINA")
library(GDINA)
set.seed(88822023)

# 1. Generate data ####
# Bonifay & Cai + Waller

#  R code for data generation 
#RC 3/2: Copied and pasted data generation code into "DataGeneration.R"
#        All you need to do now is run "source("DataGeneration.R")
source("DataGeneration.R")

# 2. Fit models ####

df <- as.data.frame(X)

## 2a. 3PL ####

mod.3pl <- 'F1 = 1-7
            PRIOR = (1-7, g, expbeta, 1, 4)' # beta(1,4) prior on guessing param
# using same prior as Bonifay & Cai 
# (see bottom of first "Estimation specifications"s paragraph, page 8)

# defaults
fit.3pl <- mirt(df, model = mod.3pl, itemtype = "3PL")

# match specifications provided in Bonifay & Cai as best we can
# NOTE: need to figure out the convergence test they mention
  # convergence test: Houts & Cai, 2013
# fit.3pl2 <- mirt(df, model = mod.3pl, itemtype = "3PL",
#                 method = "EM", optimizer = "NR",
#                 quadpts = 49, TOL = .001,
#                 control = list(maxit = 100),
#                 technical = list(NCYCLES = 2000)
# 
# )


fit.3pl
summary(fit.3pl)
coef(fit.3pl, IRTpars = TRUE, simplify = TRUE)

## 2b. DINA ####

# Q matrix: 
# for J items and K attributes (latent classes), the Q matrix is a JxK matrix
# that specifies which attributes relate to which items;
# if the attribute k is assessed by item j, the jkth element is 1
# otherwise it is 0
qmat <- matrix(c(1, 0, 0,
                 1, 0, 0,
                 1, 1, 0,
                 0, 1, 0,
                 0, 1, 1,
                 0, 0, 1,
                 0, 0, 1), nrow = 7, byrow = TRUE)

fit.dina <- GDINA::GDINA(df, qmat, model = "DINA", att.dist = "higher.order",
                          higher.order = list(model = "2PL"))

fit.dina
summary(fit.dina)
coef(fit.dina)
coef(fit.dina, what = "gs")

## 2c. DINO ####

fit.dino <- GDINA::GDINA(df, qmat, model = "DINO", att.dist = "higher.order",
                         higher.order = list(model = "2PL"))

fit.dino
summary(fit.dino)
coef(fit.dino)
coef(fit.dino, what = "gs")

## Bonus
## 2d. EIFA: exploratory 2 dimensional ####
fit.2 <- mirt(df, model = 2, itemtype = "2PL")

fit.2
summary(fit.2)
coef(fit.2, simplify = TRUE)

## 2e. bifactor ####
groupf <- c(1,1,1,1,1,2,2) # which items load on which of the 2 group factors
mod.bifactor <- 'G = 1-7
                CONSTRAIN = (6-7, a3)'
fit.bifactor <- bfactor(df, groupf, mod.bifactor)

fit.bifactor
summary(fit.bifactor)
coef(fit.bifactor, simplify = TRUE)