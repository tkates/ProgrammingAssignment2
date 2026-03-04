## These functions create a special matrix object that can cache its inverse,
## and compute/retrieve the cached inverse to avoid repeated costly computations.

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
## Returns a list containing functions to set/get the matrix and set/get its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Reset cached inverse when matrix changes
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: Computes the inverse of the special "matrix" from makeCacheMatrix.
## If the inverse is already cached, returns the cached value; otherwise computes it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
