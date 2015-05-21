## For programming assignment 2 of the R programming course at Coursera,
## create a matrix object that caches its inverse

if (!require("digest")) install.packages("digest")

## Create the caching matrix object

makeCacheMatrix <- function(x = matrix()) {
  assign("hash", c(digest(x), NULL), env = mcache)
  assign("inverse", NULL, env = mcache)
  x
}


## If the inverse of the matrix is in the cache and is not NULL 
## (and if the matrix hasn't changed), return the cached value, 
## else compute the inverse and cache and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    if(digest(x) == get("hash", envir = mcache)) {
        inv <- get("inverse", envir = mcache)
        if(is.null(inv)) {
            inv <- solve(x)
            assign("inverse", inv, env = mcache)
        }
        inv
    } else {
      x <- makeCacheMatrix(x)
      cacheSolve(x)
    }
}
