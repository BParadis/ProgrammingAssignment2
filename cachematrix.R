## As noted in the Assignment description, matrix inversion is usually a costly computation.
## Caching the inverse of a matrix can be beneficial to computing the inverse repeatedly.
## Below are two functions that create a special object to store a matrix and to cache it's inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invrs <<- inverse
    getInverse <- function() invrs
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverst of the special "matrix" returned by the makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invrs <- x$getInverse()
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    data <- x$get()
    invrs <- solve(data, ...)
    x$setInverse(invrs)
    invrs
}
