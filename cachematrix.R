## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly (there are also 
## alternatives to matrix inversion that we will not discuss here). Your assignment 
## is to write a pair of functions that cache the inverse of a matrix.


## The makeCacheMatrix function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) Inverse <<- inverse
  getInverse <- function() Inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the matrix created by the first function. 
## If the inverse is already in the cache then it calls the inverse from the cache.

cacheSolve <- function(x, ...) {
  Inv <- x$getInverse()
  if(!is.null(Inv)) {
    ## Checking to see if Inv is in the cache. If so, calls it from cache.
    message("getting cached data")
    return(Inv)
  }
  ## If Inv is null (ie. not in cache), calculate the inverse of the matrix
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInverse(Inv)
  Inv
}

