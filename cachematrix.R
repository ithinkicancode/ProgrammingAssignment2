## Matrix inversion can be a costly computation thus it might be benificial to cache the result
## of first inverse computation rather than computing it on every run.
## The following two functions, makeCacheMatrix and cacheSolve, will achieve the caching.

## makeCacheMatrix: It creates a special wrapped "matrix" object that can cache its inverse.
## makeCacheMatrix returns a list consisting of four helper functions to facilitating the caching mechanism:
## set: set or update the raw value/matrix. It also resets the cached value.
## get: retrieve the raw/original matrix
## setinverse: cache the inversion result for subsequent calls
## getinverse: retrieve the cached inversion result

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve: It computes the inverse of the special "matrix" returned by makeCacheMatrix. If the cache does not exist,
## it calls solve to get the inversion then set the cache. If the cache already exists, it simply returns the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
