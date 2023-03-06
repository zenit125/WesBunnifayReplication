#### B&C Replication: data generation functions ####

library(clusterGeneration)
library(MASS)

#### Generate random data (noise) ####

gen_BC_data <- function(){
  
# This section is the original data generation code from the appendix of ####
# Bonifay & Cai (2017)

# Number of items
nitems <- 7 

# Generate 2^7 = 128 binary response patterns
x <- list(0:1)
pattern <- expand.grid(rep(x,nitems))

# A large number
M <- 10^6

# Sample 2^nitems-1 integers 
p <- sample.int(M,size=(2^nitems-1))

# Sort in place
p <- sort(p) 

# Append the ends
p <- c(0,p,M) 

# Lag-1 difference
p <- diff(p) 

# The desired simplex
p <- p/M 

# The “sample” size 
# N = 10,000 to give the response patterns realistic frequencies
p <- p*10000 

# The weighted response pattern data
data <- cbind(pattern,p)

# This section was added by Niels Waller ####
# to converted the weighted response patter data 
# into an actual dataset of binary responses

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
  return(alldata)
}

data[,8] <- round(data[,8])
data <- data[data[,8] > 0, ]
X <- pattern2raw( pattern = as.matrix(data[ , -8]),
                  freq = data[, 8])

return(X)

}


# Generate data from the space of legitimate correlation matrices ####
gen_RanCor_data <- function(){
  # This code was suggested by Niels Waller ####
  # and updated by Joseph DeWeese
  # to generate random R matrices from the space of legitimate R matrices
  # and then generate random binary data from a randomally sampled R matrix
  # from the space of all possible nVar x nVar R matrices
  
  # We wish to generate uniformly sampled R matrices (note that the onion method does
  # not generate uniform random matrices)
  nSubj <- 10000
  nVar <- 7
  
  # Generate random R matrix
  R <- clusterGeneration::genPositiveDefMat(
    dim = nVar, 
    covMethod = "unifcorrmat", 
    alphad = 1,# alpha = 1 for uniform sampling
    rangeVar = c(1,1)
  )$Sigma
  
  # Generate random data from above R matrix
  
  X <- MASS::mvrnorm(n = nSubj, 
                     mu = rep(0,nVar), 
                     Sigma = R, 
                     empirical = TRUE)  # data will reproduce R exactly
  
  # sample random classical item difficulties
  p <- runif(nVar, 0, 1)
  # convert into thresholds
  q <- qnorm(p)
  
  Xbinary <- matrix(0, nrow = nSubj, ncol = nVar )
  for(i in 1:nVar){
    Xbinary[X[,i] <= q[i], i] <- 1
  }
  
  # Xbinary contains random binary data from a randomally sampled R matrix
  # from the space of all possible nVar x nVar R matrices
  return(Xbinary)
  
}




