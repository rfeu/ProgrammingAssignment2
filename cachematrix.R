## These two functions cache the computation to inverse a matrix, which is potentially time-consuming. computations.


## This function creates a special "vector", which is a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inversed matrix
## get the value of the inversed matrix


makeCacheMatrix <- function(x = matrix()) {
        ##Creates a special "matrix" object that can cache its inverse.
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

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
