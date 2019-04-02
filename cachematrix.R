## Put comments here that give an overall description of what your
## functions do
## This program is intended to cache the inverse of a Matrix. Usually upto 2 * 2 matrix is usually simple but is more expensive
## calculate the inverse once its more than that. Caching reduces the time and cost instead of repeated computation

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        }
}


## This function checks if the inverse has been stored in the cache post computation using the makeCacheMatrix. If yes, it returns the cache'd valued
## otherwise it calculates the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv
}
