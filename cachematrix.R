## The following functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inversex <- NULL
        set <- function(y){
                x <<- y
                inversex <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inversex <<- solve
        getinv <- function()inversex
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix 
## has not changed), then it should retrieve the inverse from the cache
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        inversex <- x$getinv()
        if(!is.null(inversex)) {
                message("getting cached data")
                return(inversex)
        }
        data <- x$get()
        inversex <- solve(data,...)
        x <- setinv(inversex)
        inversex
}
