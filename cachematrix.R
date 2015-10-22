# The following functions create a special matrix that's able to store
# the calculation of it's inverse on an internal cache.
# If provides as well a solver function that computes it's inverse and 
# gets the result from the "special matrix" in case it was previously 
# computed.


# Creates a special matrix that can cache its inverse.
#
# Args: 
#   x: A matrix (as per assignment description assumed to be always invertible).
#
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() {
        x
    }
    
    setinverse <- function(i) {
        m <<- i
    }
    
    getinverse <- function(){
        m
    }
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Calculates the inversal of a special matrix return by makeCacheMatrix.
# IF the inverse has already been calculated (and the matrix not changed),
# then the cachesolve should retrieve the inverse from the cache.
#
# Args: 
#   x: A makeCacheMatrix
#
# Returns:
#   The inverse matrix.
#
cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    m <- solve(data, ...)
    
    x$setinverse(m)
    
    m
}