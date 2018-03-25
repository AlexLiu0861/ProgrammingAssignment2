## This is a group of functions used to solve the inverse of matrix based on caching it.

## The first function is used to cache original matrix. It just likes the example given by Roger Peng.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }#This little func is used to cache X, the origin.
    get <- function() x 
    setsolve <- function(solve) m <<- solve # Use to solved cache first time.
    getsolve <- function() m #Cache the inverse if it exists, and it will be examined in the next func.
    list(set = set, get = get,
        setsolve = setsolve, getsolve = getsolve)
    #Make those four as a list, so it can be used in cacheSolve function.
}


## The next one is used to solve the inverse directly.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <<- x$getsolve()
    if (!is.null(m)) {
        message("Getting cached matrix")
        return(m)
    }# If we have solved, we can just return the final result.
    #If the cache doesn't contain the inverse, we need to solve it another time.
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
