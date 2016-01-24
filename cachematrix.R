
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# The makeCacheMatrix makes a list containing a function in order to
# set and get the value of the matrix.
# And set and get the value of inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## The function below returns the inverse of the matrix. It will first check if
# the inverse has already been computed. Then, if so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function will always assumes that the matrix is invertible.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting the cached data.")
        return(inv)
    }
    matrixdata <- x$get()
    inv <- solve(matrixdata)
    x$setinverse(inv)
    inv
}
