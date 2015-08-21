## R Programming - Assignment 2

## Function "makeCacheMatrix" creates a special "matrix" object that can cache
## its inverse.
## Function "cacheSolve" computes the inverse of the special "matrix" returned
## by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

## Usage example:
#> source("cachematrix.R")
#> n <- 4L					        ## initiate number of rows & columns of m
#> m <- matrix(rnorm(n^2), n, n)	## creates a random n-by-n matrix from
# standard normal dist
#> cm <- makeCacheMatrix(m)		## creates matrix that can cache its inverse
#> cacheSolve(cm)				## returns inverse of m, assuming it's invertible
#> cacheSolve(cm)				## rerun to see that cacheSolve will retrieve the
#> cached inverse

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    ## sets the value of the matrix and initiates its inverse with NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    ## gets the value of the matrix
    get <- function() x
    ## sets the value of the inverse
    setinv <- function(inverse) xinv <<- inverse
    ## gets the value of the inverse
    getinv <- function() xinv
    ## returns a list containing all those objects above
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    ## Gets the inverse and checks if it's already cached (not null)
    ## if it's already cached, then it this function will stop and return the
    ## cached data.
    ## The inverse will be set to NULL automatically if the the matrix is changed
    ## (refer to x$set).
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    ## If the matrix has changed, gets the new matrix returned by makeCacheMatrix
    data <- x$get()
    ## using "solve(matrix)" to find its inverse
    xinv <- solve(data, ...)
    ## sets the inverse
    x$setinv(xinv)
    ## Returns a matrix that is the inverse of 'x'
    xinv
}
