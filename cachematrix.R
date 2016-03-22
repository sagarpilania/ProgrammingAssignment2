## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  getInv <- function() inv
  setInv <- function(inverse) inv <<- inverse
  list(set = set, get = get, getInv = getInv, setInv = setInv)
}


## This function returns the cached value of the supplied matrix if available or calculates the matrix inverse value
## and sets the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
