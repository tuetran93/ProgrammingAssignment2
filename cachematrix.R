## These functions will perform matrix inversion on an invertible matrix
## and cache the result, so that R doesn't have to redo this at every run
## of a program.

## makeCacheMatrix creates a list of functions, which save the result of
## the matrix inversion and enable us to access this result as needed.

makeCacheMatrix <- function(x = matrix()) {
        if(!is.matrix(x)) {
                stop("only accept matrices as arguments")
        }
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvert <- function(invert) m <<- invert
        getinvert <- function() m
        list(set = set, get = get, 
             setinvert = setinvert, getinvert = getinvert)
}


## cacheSolve uses the functions in makeCacheMatrix to invert the input 
## matrix and save the output. If an inverted matrix is already produced,
## cacheSolve will not repeat the inversion.

cacheSolve <- function(x, ...) {
        m <- x$getinvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinvert(m)
        m
}
