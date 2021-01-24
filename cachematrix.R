## Put comments here that give an overall description of what your
## functions do
## These functions caches the inverse of a matrix.
## Within the first function, the value of the matrix can be obtained with \code{m$get()}.
## The value of the matrix can be changed with \code{m$set(y)} where y is an ordinary matrix.
## The inverse can be obtained with \code{cacheSolve(m)}.

## Write a short comment describing this function
## This function creates a list containing four functions to set and get the value of the matrix and its inverse

makeCacheMatric <- function(x=matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function () x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix from the function above. If the inverse has already been calculated,
## and the matrix has not changed, then the cachesolve retrieves the inverse from the cache. Otherwise, the inverse
## will be calculated with the new matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
        ## Return a matrix that is the inverse of 'x'
