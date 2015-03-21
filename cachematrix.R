## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    message("calculating data")

    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

testCache <- function(N) {
	x <- matrix(rnorm(N*N,mean=5,sd=1), N, N)
    cm<-makeCacheMatrix(x)
    cacheSolve(cm)
    cacheSolve(cm)
    'done'
}