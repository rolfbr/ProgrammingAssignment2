## Below are two functions that create a matrix and store its inverse
## in cache so that it can be retrieved from cache and reused without
## having to recalculate.

## The function makeCacheMatrix creates an R object that stores a
## matrix and its inverse.

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
             setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve requires an argument that is returned by
## makeCacheMatrix in order to retrieve the inverse from the cached
## value that is stored in the makeCacheMatrix object's environment,
## or if it's not already available in cache - calcuate its inverse
## and store it in cache.
## NOTE: this function assumes the supplied matrix is always invertible!

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}