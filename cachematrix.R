## The two functions are used to cache the inverse of a matrix. 
## This function creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function calcalutes the inverse of the "matrix" created by madecachematrix function 
## If the inverse has been computed, the result will appear. If not, it has a computation step for the inverse.
cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("Getting cached data.")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinv(inverse)
        inverse
}
