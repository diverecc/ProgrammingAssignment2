## In order to optimise computation time here we create a matrix enveloppe that can store 
## the value of am inverted matrix for a given matrix

## makeCacheMatrix caches the inverted value of the matrix passed in argument
## By default an empty matrix is created if no matrix is explicitly passed
makeCacheMatrix <- function(x = matrix()) {
    invert <- NULL
    set <- function(y) {
        x <<- y
        invert <<- NULL
    }
    ## Defines 4 functions for this object
    ## get() to return the value of the matrix
    ## set() to set the value of the matrix
    ## setsolve() to compute the invert and store it in invert
    ## getsolve() to return the stored inverted matrix
    get <- function() x
    setsolve <- function(solve) invert <<- solve
    getsolve <- function() invert
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve computes the invert of the matrix passed in argument the first time being called
## If called again, the previously computed result will be returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invert <- x$getsolve()
    if(!is.null(invert)) {
        message("getting cached data")
        return(invert)
    }
    data <- x$get()
    invert <- solve(data, ...)
    x$setsolve(invert)
    invert
}


makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}