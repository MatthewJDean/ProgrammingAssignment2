## Filename:
##      cachematrix.R
##
## Description:
##      The cachematrix script contains two functions that are used together to calculate and cache the inverse of a matrix.
##
## Sample usage:
##      sampleMatrix <- diag(3,7)                       # Creates a 7x7 square matrix
##      cachedMatrix <- makeCacheMatrix(sampleMatrix)   # Creates the special list matrix
##      cacheSolve(cachedMatrix)                        # Outputs the inverse of the matrix and caches the result
##      cacheSolve(cachedMatrix)                        # Subsequent calls retreive the inverse value from the cache instead of recalculating

makeCacheMatrix <- function(x = matrix()) {
    # Creates a special "matrix" object that can cache its inverse.
    #
    # Args:
    #   x: A square invertible matrix.
    #
    # Returns:
    #   A list containing functions to
    #       1. set the matrix
    #       2. get the matrix
    #       3. set the inverse
    #       4. get the inverse
    #   This list is used as the input to cacheSolve().
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    # This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has
    # already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
    #
    # Args:
    #   x: The output of makeCacheMatrix().
    #
    # Returns:
    #   Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        message("** Using cached data **")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}

