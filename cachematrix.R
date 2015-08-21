## R Programming - Assignment 2

## Function "makeCacheMatrix" creates a special "matrix" object that can cache
## its inverse.
## Function "cacheSolve" computes the inverse of the special "matrix" returned
## by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

## Usage example:
## source("cachematrix.R")
## n <- 4L					        ## initiate number of rows & columns of m
## m <- matrix(rnorm(n^2), n, n)	## creates a random n-by-n matrix from
                                    ## standard normal dist
## cm <- makeCacheMatrix(m)		## creates matrix that can cache its inverse
## cacheSolve(cm)				## returns inverse of m, assuming it's invertible
## cacheSolve(cm)				## rerun to see that cacheSolve will retrieve the
                                ## cached inverse

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) xinv <<- inverse
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {

    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv		## Return a matrix that is the inverse of 'x'
}
