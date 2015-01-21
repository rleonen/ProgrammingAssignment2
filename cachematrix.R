## The makeCacheMatrix function will calculate the inverse of a given matrix
## and cache the returned inverse for repeated use in order to avoid 
## repeated matrix inversion calculations. The cacheSolve function will 
## request the calculation of a new matrix inversion or retrieve the 
## cached results of a previous matrix inversion calculation.

## takes input matrix x and calculates the inverse using the solve() function
## Saves the resulting inversion for later use.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolveMatrix <- function(solve) m <<- solve
    getSolveMatrix <- function() m
    list(set = set, get = get,
         setSolveMatrix = setSolveMatrix,
         getSolveMatrix = getSolveMatrix)
}


## requests the inversion of x from the makeCacheMatrix function.
## if inversion already exists, cache version is returned. Otherwise
## new inversion is caculated and cached by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getSolveMatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setSolveMatrix(m)
    m
}
