## This set of functions together allows someone to
## create a special matrix, cache its inverse, and then
## use that matrix's inverse when needed by looking at the
## cached value.  Thus the inverse is only computed 1 time
## (assuming the matrix stays the same) and can be referenced
## repeatedly without needing to recompute it.

## Takes a matrix x and returns a wrapper that
## allows for x's inverse to be cached on it.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Takes a special matrix wrapper created by makeCacheMatrix
## and uses it to computer that matrix's inverse (or look it up
## in its cache).

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}