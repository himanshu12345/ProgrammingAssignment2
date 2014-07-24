## Matrix inversion is a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. The following pair of 
## functions cache the inverse of a matrix.

## makeCacheMatrix creates a special matrix object that can cache its inverse.
## The special object is a list of functions for setting and/or retrieving a matrix,
## and its inverse.

makeCacheMatrix <- function(x = matrix(0)) {
     inv <- NULL
     set <- function(y) {
          x <<- y        ## Function for setting the matrix value.
          inv <<- NULL
     }
     get <- function() x           ## Function for getting the cached matrix value.
     setinv <- function(inverse) inv <<- inverse       ##Function for setting the inverse.
     getinv <- function() inv           ## Function for getting the cached value of inverse.
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)         ## Returns the functions as a list.
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)         ## Find and return the inverse if already computed.
     }
     data <- x$get()
     inv <- solve(data, ...)       ## Compute the inverse if not found cached.    
     x$setinv(inv)            
     inv       ## Return a matrix that is the inverse of 'x'.
}


