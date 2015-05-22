## The functions makeCacheMatrix and cacheSolve allow to create
## a Matrix that can cache its inverse to avoid the unnecesary
## computation of calculating an inverse over again.

## makeCacheMatrix creates a list of functions that can be
## called to manipulate the matrix and the inverse. Most
## importantly, it resets the value of the inverse upon setting
## a new Matrix. It must be called before using cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        
        list(
                set = set, 
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## cacheSolve determines if an inverse matrix has already
## been calculated for the matrix defined by makeCacheMatrix.
## If an inverse matrix has been calculated, it is returned as
## the result. If no inverse matrix is present in the list, 
## an inverse matrix is calculated and stored to be reused
## on further calls of cacheSolve.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        
        inverse
}
