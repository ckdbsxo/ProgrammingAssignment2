## These functions cache potentially time-consuming computations, such as
## a very long vector, especially if has to be computed repeatedly (e.g. in a loop).

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinvmtx <- function(inv_mtx) m <<- inv_mtx
    getinvmtx <- function() m
    list(set = set, get = get,
         setinvmtx = setinvmtx,
         getinvmtx = getinvmtx)
}


## TL; DR: Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    m <- x$getinvmtx()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvmtx(m)
    m
}
