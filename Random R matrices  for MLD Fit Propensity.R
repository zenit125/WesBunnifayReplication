
###################################################################
## Niels Waller March 1, 2023
## Some code for the MDL Project
##  The Space of 3 x 3 R matrices and random data R matrices
##  elliptical tetrahedron
##  All 3 x 3 R matrices with |R| = 0
###################################################################

library(rgl)

# Define functions to generate points on the surface of
# an elliptical tetrahedron
r1 <- r2<-seq(-1,1,by=.01)
f1<-function(r1,r2){
  sqrt ((1-r1^2) * (1-r2^2)) + r1*r2
}

f2<-function(r1,r2){
  -sqrt ((1-r1^2) * (1-r2^2)) + r1*r2
}


# Plot the elliptical tetrahedron

# View the elliptope from a nice perspective
zoom<-1
#userMatrix<-par3d()$userMatrix
#windowRect<-par3d()$windowRect
userMatrix <- matrix(c(
  0.8154715,  0.5787458, -0.007708319,    0,
  -0.1117193,  0.1704556,  0.979011655,    0,
  0.5679128, -0.7974948,  0.203658789,    0,
  0.0000000,  0.0000000,  0.000000000,    1),4, 4, byrow=TRUE)

windowRect <- c( 0,  45, 836, 617)
open3d(zoom = zoom, userMatrix = userMatrix, windowRect=windowRect)
par3d(cex=1.4)
material3d(col="red")



persp3d(r1,r2,outer(r1,r2,f1), 
        col="wheat", 
        xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1),
        xlab="rXY", ylab="rXZ", zlab="rYZ")
persp3d(r1,r2,outer(r1,r2,f2), col="wheat", add=TRUE)

# Note that the generated figure can be rotated by dragging 
# your cursur across the rgl object.


##==================================

# n = sample size
# we want to generate random R matrices from a population I matrix

Nsamples <- 5E5   # generate R from random data
# save upper triangle of 3 x 3 R matrix
r12r13r23 <- matrix(0, nrow = Nsamples, ncol = 3)

for( i in 1:Nsamples){
    R <- fungible::corSample(R = diag(3), n = 1000)$cor.sample
    r12r13r23[i, ] <- R[upper.tri(R, diag=FALSE)]
}

plot3d(r12r13r23[,1],
        r12r13r23[,2],
        r12r13r23[,3],
        col="black", 
        xlim=c(-1,1), ylim=c(-1,1), zlim=c(-1,1),
        xlab="rXY", ylab="rXZ", zlab="rYZ")


# How to generate random R matrices from the space of legitimate 
# R matrices

# We wish to generate uniformly sampled R matrices (note that the onion method does
# not generate uniform random matrices)
library(clusterGeneration)


set.seed(123)
nSubj <- 1000
nVar = 3

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

# check
cor(X) - R

# sample random classical item difficulties
p <- runif(nVar, 0, 1)
# convert into thresholds
q <- qnorm(p)

Xbinary <- matrix(0, nrow = nSubj, ncol = nVar )
for(i in 1:nVar){
  Xbinary[X[,1] <= q[i], i] <- 1
}

# check
apply(Xbinary, 2, mean)
p

# Xbinary contains random binary data from a randomally sampled R matrix
# from the space of all possible nVar x nVar R matrices












