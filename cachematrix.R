## Set of functions that defines a matrix-like object whose inverse
## can be cached, and the function that utilizes the caching.

## Defines a matrix objects that allows for cached values of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        cacheinverse <- NULL
        set <- function(y) {
                x <<- y
                cacheinverse <<- NULL
        }
        get <- function() x
        setinv <- function(solve) cacheinverse <<- solve
        getinv <- function() cacheinverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Checks the CacheMatrix 'x' for a cached inverse and returns it,
## Otherwise, solves for the inverse and stores it in cacheinverse.

cacheSolve <- function(x, ...) {
        cacheinverse <- x$getinv()
        if(!is.null(cacheinverse)) {
                message('getting cached data')
                return(cacheinverse)
        }
        data <- x$get()
        cacheinverse <- solve(data, ...)
        x$setinv(cacheinverse)
        cacheinverse
}
