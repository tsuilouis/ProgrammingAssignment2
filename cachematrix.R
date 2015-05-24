## Put comments here that give an overall description of what your
## functions do
#  makeCacheMatrix() returns a list of functions to
#  1) set the value of the matrix
#  2) get the value of the matrix
#  3) set the value of the matrix inverse
#  4) get the value of the matrix inverse
#
#  cacheSolve() calculates the inverse of the special "matrix"
#  created with makeCacheMatrix(). It checks to see if the 
#  inverse has already been calculated. If so, it gets the
#  inverse from the cache and skips the computation. Otherwise,
#  it calculates the inverse of the matrix and sets the value
#  of the inverse in the cache via the setInverse() function.
#  
#  As per assignment instructions, assume the matrix supplied 
#  is always invertible

## Write a short comment describing this function
# This function creates a special "matrix" object that 
# can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     get <- function() {
          x
     }
     setInverse <- function(inv) {
          inverse <<- inv
     }
     getInverse <- function() {
          inverse
     }
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has 
# already been calculated (and the matrix has not changed),
# then the cacheSolve should retrieve the inverse from the 
# cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inverse <- x$getInverse()
     if(!is.null(inverse)) {     # inverse is cached
          message("getting cached data")
          return(inverse)
     }
     data <- x$get()
     inverse <- solve(data, ...)     # calculates inverse
     x$setInverse(inverse)
     inverse
}
