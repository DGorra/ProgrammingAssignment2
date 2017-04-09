## Matrix inversion is a costly computation therefore is 
## a good idea caching the inverse instead of making the 
## the calculation each time it's needed. 

## This function creates an special object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse of the special 
## object created by makeCacheMatrix, if the inverse has
## been calculated then it returns the inverse from the 
## cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
