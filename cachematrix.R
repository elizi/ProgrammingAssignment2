## These functions cache the inverse of a matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function computes the inverse of the special matrix returned by the makeCacheMatrix function
## If the inverse has already been calculated, then the function will retrieve the inverse from cache

cacheSolve <- function(x, ...) {
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("Getting cached data...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}
