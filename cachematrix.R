## Two functions have been implemented: 
##      makeCacheMatrix:    creates a special matrix with the necessary methods
##                          to access it and its inverse
##      cacheSolve:         computes and caches the inverse of the special matrix created by makeCacheMatrix


## The makeCacheMatrix function creates a special matrix with the
## following four methods to access its contents:
## - set: sets the matrix.
## - get: gets the matrix.
## - setInverse: sets the matrix inverse.
## - getInverse: gets the matrix inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    mInv <- NULL
    
    # Sets up the functions to access the matrix and its inverse
    set <- function(y) {
        x <<- y
        mInv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) mInv <<- inverse
    getInverse <- function() mInv
    
    # Returns a list object containing the matrix functions built above
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve function gets in input a special matrix object
## created by the makeCacheMatrix function and return its inverse
## using the cache, if possible, to avoid unnecessary computations.
cacheSolve <- function(x, ...) {
    # Tries to get the inverse
    mInv <- x$getInverse()
    
    # If the inverse is already computed, 
    # then return the cache's content
    if(!is.null(mInv)) {
        message("Getting cached data")
        return(mInv)
    } else {
        # Compute the inverse and
        # set its value in the cache
        mData <- x$get()
        mInv <- solve(mData, ...)
        x$setInverse(mInv)
        
        return(mInv)
    }
}
