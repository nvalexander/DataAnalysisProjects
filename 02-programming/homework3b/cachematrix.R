## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix-like variable, containing, besides
#  usual matrix components, a cached inverse of the main matrix.
#  This function returns a list of four functions, within whom the data 
#  are actually contained.
#  The matrix itself is stored as x, but in order to read and write x, 
#  you must use makeCacheMatrix(somematrix)$get and 
#  makeCacheMatrix(somematrix)$set.
#  Similarly, the inverse is stored internally as inv, but read and 
#  write it, you must use setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() { x }
  setinverse <- function(inverse) { inv <<- inverse }
  getinverse <- function() { inv }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function calculates the inverse of matrix-like variable.
#  If the matrix-like variable already has a cached inverse, it returns it.
#  Otherwise, it does the inverse calculation, adds that to the cache, 
#  and returns the calculated inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
