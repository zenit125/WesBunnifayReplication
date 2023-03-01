#### Demo: Fit 3PL, DINA, and DINO models ####
## 2023-02-27; Joseph N. DeWeese
# 
# use Bonifay & Cai + Niels Waller code to generate data
# fit 3pl with mirt, fit DINO and DINA with GDINA
# I also ended up fitting the EIFA and bifactor models in mirt


# 0. setup ####
library(mirt)
library(GDINA)
set.seed(88822023)

# 1. Generate data ####
# Bonifay & Cai + Waller

#  R code for data generation 

# Number of items
nitems <- 7 

# Generate 2^7 = 128 binary response patterns
x <- list(0:1)
pattern <- expand.grid(rep(x,nitems))

#NW
dim(pattern)
head(pattern)

# A large number
M <- 10^6

# Sample 2^nitems-1 integers 
p <- sample.int(M,size=(2^nitems-1))

#NW 
# p is 127 random samples without replacement from
# 1 : 1 million

# Sort in place
p <- sort(p) 

# Append the ends
# NW because we are appending M to the end of the matrix the 
# size is 2^nitems - 1 + 1 = 2^7
p <- c(0,p,M) 

# Lag-1 difference
p <- diff(p) 

# The desired simplex
p <- p/M 

#NW This will equl 1
sum(p)

# The “sample” size 
# N = 10,000 to give the response patterns realistic frequencies
p <- p*10000 

# The weighted response pattern data
data <- cbind(pattern,p)

#NW 
sum(p)
sum(round(p))

head(data)
data[1:128,]

#NW pattern2raw
# We want to create a raw data matrix from the response frequencies
pattern2raw <- function(pattern,freq){
  npattern <- nrow(pattern)
  nitem <- ncol(pattern)
  alldata <- matrix(0, nrow=sum(freq), ncol=nitem)
  rowbegin <- 0
  rowend <- 0
  for(i in 1:npattern){
    alldata[(rowbegin+1):(rowend+freq[i]), ]<-matrix(pattern[i,],
                                                     nrow=freq[i],
                                                     ncol=nitem,
                                                     byrow=TRUE)
    
    rowbegin <- rowbegin+freq[i]
    rowend <- rowend+freq[i]
  }   
  alldata
}

data[,8] <- round(data[,8])
data <- data[data[,8] > 0, ]
X <- pattern2raw( pattern = as.matrix(data[ , -8]),
                  freq = data[, 8])
dim(X)

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