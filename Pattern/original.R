# Number of cells in a row not matching a simple pattern
#
# x: numeric data.frame  or matrix
# pattern: row pattern to detect, vector of length(ncol(x)) with
# entry : meaning
#  -1   : < 0
#   1   : > 0
#   NA  : NA 
#   0   : 0
#
patternDist <- function(x,pattern){
  apply(sign(x), 1,
        function(row){
          v <- row - pattern
          v[is.na(pattern) & is.na(row)] <- 0
          v[xor(is.na(pattern), is.na(row))] <- 1
          sum( v != 0 )
        })
}

set.seed(1)
x <- matrix(rnorm(40),nrow=10)
x[sample(40,4)] <- 0
x[sample(40,4)] <- NA

x <- as.data.frame(x)
x

#count of columns not matched the pattern
patternDist(x,pattern=c(-1,0,1,NA))
#cat("\014")
