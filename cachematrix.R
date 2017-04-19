## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly. The below functions does the job of caching the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It creates a special "matrix", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverted matrix
## get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        inverted <- NULL
        set <- function(y) {
                x <<- y
                inverted <<- NULL
        }
        get <- function() x
        setinverted <- function(solve) inverted <<- solve
        getinverted <- function() inverted
        list(set = set, get = get,
             setinverted = setinverted,
             getinverted = getinverted)
}


## This function calculates the inverted matrix of the special "matric" created with the makeCacheMatrix function. 
## However, it first checks to see if the inverted matrix has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the inverted matrix of the given matrix and sets the inverted matrix in 
## the cache via the setinverted function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverted <- x$getinverted()
        if(!is.null(inverted)) {
                message("getting cached data")
                return(inverted)
        }
        data <- x$get()
        inverted <- solve(data)
        x$setinverted(inverted)
        inverted
}
