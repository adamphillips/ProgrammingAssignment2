## cachematrix.R
##
## Functions for caching the inverse of a matrix
## These two functions are designed to be used in conjunction to solve a matrix
## and cache the result to avoid repeated calculation.
##
## Example:
##  cm = makeCacheMatrix(matrix(1:4), nrow=2, ncol=2)
##  cacheSolve(cm) # first call calculates the matrix, stores in the cache and returns the result
##  cacheSolve(cm) # subsequent calls return the cached value without recalculation

## Creates a structure holding the matrix whose inverse is to be cached.
## Takes the matrix to be examined as the first argument.
## Returns a list containing 4 functions for working with the cacheable matrix.
##
## - set()        : sets the value of the matrix
## - get()        : returns the value of the matrix
## - setInverse() : sets the cached inverse value
## - getInverse() : returns the cached inverse value
##
## Note that this function is not responsible for solving the matrix. This is
## handled by the cacheSolve() function.
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL

    set <- function(value) {
        x <<- value
        cachedInverse <<- NULL
    }

    get <- function() x

    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse

    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## Takes the list returned from a call to makeCacheMatrix, solves the matrix
## and stores the value in the cache.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()

    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }

    data <- x$get()
    inverse <- solve(data, ...)

    x$setInverse(inverse)
    inverse
}
