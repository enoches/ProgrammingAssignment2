# Exercise: this code caches results of matrix inversion in order to save time
# on CPU-intensive tasks, where the same inverted matrix is needed multiple times.

# The function bellow creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# This function returns the inverse of a matrix:
# First it checks if a cached result exists. If it does, a result from cache is returned.
# If not it makes actual computation and caches result. 

cacheSolve <- function(x, ...) {
        ## Returning a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
