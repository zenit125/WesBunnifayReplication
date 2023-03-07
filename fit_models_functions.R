#### B&C Replication: model fitting functions ####
# wrapper functions to fit the 5 modles specified in Boniday & Cai (2017)
  # 1. EIFA: Exploratory item factor analysis 
  # 2. Bifactor model
  # 3. DINA: deterministic input noisy and-gate
  # 4. DINO: deterministic input noisy or-gate
  # 5. unidimensional 3PL
# each is for 7 observed items, with a particular specification
# first four have 20 parameters, 3PL has 21
# for initial exploratory simulations, mostly use package defaults
# may need to mess with estimation paramters later


# packages
library(mirt)
library(GDINA)

# fit EIFA ####
# 2 factor (dimensional) model
fit_eifa <- function(data){
  
  # loading of factor 2 to item 7 constrained to zero for model identification purposes
  # this is the default; for this analysis it doesn't matter which loading we constrain
  fit.eifa <- mirt(data, model = 2, itemtype = "2PL")
  
  return(fit.eifa)
  
}


# fit bifactor model ####
groupf <- c(1,1,1,1,1,2,2) # which items load on which of the 2 group factors
mod.bifactor <- 'G = 1-7
                CONSTRAIN = (6-7, a3)' # identification constraint of B&C

fit_bifactor <- function(data, mod1 = groupf, mod2 = mod.bifactor){
  
  fit.bifactor <- bfactor(data, model = mod1, model2 = mod2)
  
  return(fit.bifactor)
}


# fit DINA ####
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

fit_dina <- function(data, q = qmat){
  
  # B&C use a heirarchical factor model with a higher order 2PL
  # the binary latent attributes relate to a higher order continuous trait
  # and that relationship is modeled with a 2PL
  # I believe they choose this particular specification just so it has 20 params
  fit.dina <- GDINA::GDINA(data, Q = q, model = "DINA", 
                           att.dist = "higher.order",
                           higher.order = list(model = "2PL"))
  
  return(fit.dina)
}


# fit DINO ####
# uses same Q matrix as DINA model

fit_dino <- function(data, q = qmat){
  
  # same notes as DINA model
  # B&C use a heirarchical factor model with a higher order 2PL
  # the binary latent attributes relate to a higher order continuous trait
  # and that relationship is modeled with a 2PL
  # I believe they choose this particular specification just so it has 20 params
  fit.dino <- GDINA::GDINA(data, Q = q, model = "DINO", 
                           att.dist = "higher.order",
                           higher.order = list(model = "2PL"))
  
  return(fit.dino)
}


# fit unidimensional 3PL ####
# 3PL with guessin param prior as specified by B&C
# (see bottom of first "Estimation specifications"s paragraph, page 8)
mod.3pl <- 'F1 = 1-7
            PRIOR = (1-7, g, expbeta, 1, 4)' # beta(1,4) prior on guessing param

fit_3pl <- function(data, mod = mod.3pl){
  
  fit.3pl <- mirt(data, model = mod, itemtype = "3PL")
  
  return(fit.3pl)
}


# fit all models ####
# return a list (of length 5) with each fitted model object
fit_all_models <- function(data){
  
  m.eifa <- fit_eifa(data)
  m.bifactor <- fit_bifactor(data)
  m.dina <- fit_dina(data)
  m.dino <- fit_dino(data)
  m.3pl <- fit_3pl(data)
  
  m.list <- list(m.eifa = m.eifa,
                   m.bifactor = m.bifactor,
                   m.dina = m.dina,
                   m.dino = m.dino,
                   m.3pl = m.3pl)
  
  return(m.list)
}

