## makeCacheMatrix takes a matrix as an input and output a list object
## caching the input matrix and its inverse
## To take advantage of this function with any matrix, as a first step, assign the output
## of this function to a variable.
## Then, when you need to calculate its inverse, invoke cacheSolve on the output of makeCacheMatrix

## The cache environment is created in the global environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve takes as input a list object generated with makeCacheMatrix
## If the Inverse Matrix is found cached, it's returned otherwise is calculated
## using the Solve function , cached for future reference and finally returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
