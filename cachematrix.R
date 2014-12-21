# There are two functions: makeCacheMatrix and cacheSolve.
# Together they provide an efficient way to save a matrix and its inverse
# so that the inverse is never recalculated unless the matrix is changed.


# makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # inv stores the cached inverse of the matrix x
        inv <- NULL
        # function to set a new matrix x
        set <- function(y) {
                # matrix has been reset, so clear the cache
                x <<- y
                inv <<- NULL
        }
        # function to get the matrix x
        get <- function() x
        # function to set the cached inverse of x
        setinverse <- function(inverse) inv <<- inverse
        # function to get the cached inverse of x
        getinverse <- function() inv
        # list of available functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cacheSolve should retrieve the inverse from the cache.

# Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        # first try to get the cached inverse
        inv <- x$getinverse()
        if(!is.null(inv)) {
                # inverse has already been calculated
                message("using cached data")
                # return cached result
                return(inv)
        }
        # inverse not cached so calculate it
        message("not cached, so calculating inverse")
        data <- x$get()
        inv <- solve(data, ...)
        # cache the result for future use
        x$setinverse(inv)
        inv
}
