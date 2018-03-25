## This is a group of functions used to solve the inverse of matrix based on caching it.

## The first function is used to cache original matrix. It just likes the example given by Roger Peng.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
        setsolve = setsolve , getsolve = getsolve)
}


## The next one is used to solve the inverse directly.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <<- x$getsolve()
    if (!is.null(m)) {
        message("Getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
