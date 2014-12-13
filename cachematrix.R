# The functions below help improve the performance of potentially expensive 
# operations by caching their results
# The first function, `makeVector` creates a special "vector", which is
# really a list containing functions
#
# 1.  set the value of the vector
# 2.  get the value of the vector
# 3.  set the value of the mean
# 4.  get the value of the mean
#
# The second function executes the actual operation and caches the result if 
# not already existing, and returns the cached results otherwise

## creates a list containig functions and the eventually cached result

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## performs an inverse operation on a matrix and caches the result if not already 
# cached, returns the latter otherwise

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(a = data, ...)
    x$setinverse(inv)
    inv
}
