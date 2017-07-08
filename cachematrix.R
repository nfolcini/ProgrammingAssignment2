## Below are two functions that are used to cache the inverse of a matrix because this is an intensive calculation## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                m <<- y
                inv <<- NULL
        }
        get <- function() m
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
        inv <- m$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- m$get()
        inv <- solve(data, ...)
        m$setInverse(inv)
        inv
}

