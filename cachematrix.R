## This script contains two functions: makeCacheMatrix and cacheSolve
## It is aimed to store the cached value of a matrix, in order to speed up
## the calculations when utilizing the same matrix several times.

## Function to cache-ing the matrix so we can calculate the inverse
makeCacheMatrix <- function(x= matrix()) {
  #cleaning the cache
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #define internal functions
  get <- function()x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Function to Solve the matrix (calculate the inverse) if it hasn't been solved 
## Otherwise retrieve cached inverse.
## Note: Function is valid only if the matrix is squared. Using "solve()" 
## function for calculating the inverse.

cacheinv <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', according to:
  i <- x$getinv()
  ## Check if inverse is stored already
  if(!is.null(i)) {
    ## If it is stored, getthe cached data
    message("getting cached data")
    return(i)
  }
  ## If it is not stored, calculate the inverse with "solve()"
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i 
}
