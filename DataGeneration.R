
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