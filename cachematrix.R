## Put comments here that give an overall description of what your
## functions do

## These functions invert a matrix, and cache the result. If the same matrix is 
## submitted for inversion again, the cached result will be returned.

## Write a short comment describing this function
## Creates a list of functions, calculating the inverse of the matrix x, and caching it 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## Checks if the inverted matrix has already been cached. If not, inverts the matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
