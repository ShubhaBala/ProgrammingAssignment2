## This program will use the cache to store a matrix and compute it's inverse

## makeCacheMatrix creates a "special" matrix object and stores it in cache, as well as stores its inverse in cache.
## It contains functions to set your matrix, get it from cache, set the inverse in cache, and get the inverse from cache

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve takes in a list output from makeCacheMatrix and returns the inverse - retrieving it from cache if it already exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## calculates and stores the inverse if it has not already been calculated and stored in cache
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
