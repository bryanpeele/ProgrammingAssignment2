## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates special "matrix" object that can cache its inverse
## Input matrix is assumed to be invertible
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getinverse = getInverse)
}


## Write a short comment describing this function
## Computes the inverse of "matrix" returned by makeCacheMatrix
## Retrieves inverse if already calculated
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
