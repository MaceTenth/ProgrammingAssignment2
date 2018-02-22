## this function is design for setting the value of the matrix 
## then receive the value of the matrix.
## In Addition, setting the value of the inverse of the matrix 
## then acquire the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function returns the inverse of the matrix. It checks if calculation of
## the inverse already took If true, it gets the result and skips the computation.
## If false it figures the inverse, sets the value in the cache with setinverse function.


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)#solve() will return the inverse of matrix data
  x$setinverse(inv)
  inv
}
