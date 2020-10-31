## Programming Assignment 2
## Two R functions to cache inverse of a matrix 

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}


# Code to test function -------------------------------------------------------

c<-matrix(data=1:4, nrow = 2, ncol = 2) ## creates a test matrix
test <- makeCacheMatrix(c)  ## applies make first function to test matrix
test$get()              ## calls the get function from makeCacheMatrix
test$getInverse()
cacheSolve(test)
cacheSolve(test)



