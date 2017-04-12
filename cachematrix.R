## Matrix inversion is a costly computation therefore is 
## a good idea caching the inverse instead of making the 
## the calculation each time it's needed. 

## This function creates an special object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL                                 #creating an empty inverse
  set <- function (y) {                           #setting the matrix (NOT inverse)
    x <<- y
    inverse <<- NULL
  }
  get <-function () x                             #gets the matrix
  setinv<- function (inverse) inv <<- inverse     #manually set inverse
  getinv <- function () inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function calculates the inverse of the special 
## object created by makeCacheMatrix, if the inverse has
## been calculated then it returns the inverse from the 
## cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()               #checks cache to get inverse of x
  if (!is.null(inv)) {                #if already calculated, it is =! NULL
    message("getting cached data")    #lets you know it's getting it from cache
    return(inv)                       #returns inv from cache
  }
  mat <- x$get()                      #inverse was not calculated, so: it gets the matrix
  inv <- inverse(mat, ...)            #calculates the inverse of the matrix
  x$setinverse(inv)                   #sets inverse as inv (to store in cache)
  inv                                 #returns inverse
}