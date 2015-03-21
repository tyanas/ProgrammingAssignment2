# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing it
# repeatedly. One can find here a pair of functions that cache the inverse
# of a matrix.


# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # reset cache
    inv <- NULL
    set <- function(y) {
        # set base matrix
        x <<- y
        # clear cache
        inv <<- NULL
    }
    get <- function() x
    # update cache
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv

    # "matrix" object interface
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned
# by `makeCacheMatrix` above. If the inverse has already been calculated
# (and the matrix has not changed), then `cacheSolve` will retrieve
# the inverse from the cache.

# Assume `X` is a square invertible matrix

cacheSolve <- function(x, currentX, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    # first check if we have already cached object
    # second (not evaluated when cache is empty) check are there any changes in
    # in base matrix comared to current matrix
    if(!is.null(inv) && all(x$get() == currentX)) {
        message("getting cached data")
        return(inv)
    }
    message("calculating data")
    x$set(currentX)
    # caclucate inversed matrix here
    inv <- solve(currentX, ...)
    x$setinverse(inv)
    inv
}

testCache <- function(N) {
    # test how it goes
    x <- matrix(rnorm(N*N, mean=5, sd=1), N, N)
    # no calculations, just define an object
    cm <- makeCacheMatrix(x)
    # must be 'calculating data'
    cacheSolve(cm, x)
    # must be 'getting cached data'
    cacheSolve(cm, x)
    # change base matrix a bit
    x[2,1] <- 10
    # must be 'calculating data'
    cacheSolve(cm, x)
    # must be 'getting cached data'
    cacheSolve(cm, x)
    'done'
}