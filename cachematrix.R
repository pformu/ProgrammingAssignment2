## The following functions show the cache concept to avoid the unnecessary recalculation 
## of time-consuming mathematical functions

## makeCacheMatrix creates a special "matrix" object that will be able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
       i <- NULL
       get <- function() x
       setinv <- function(inv) i <<- inv
       getinv <- function() i
       list(get = get,
            setinv = setinv,
            getinv = getinv) 
}


## cacheSolve computes the inverse of the special "matrix" returned 
## by above function makeCacheMatrix;
## note that if the inverse matrix has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
       i <- x$getinv()
       if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i   
}
