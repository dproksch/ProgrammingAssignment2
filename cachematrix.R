## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## sets up the ability to cache & return a matrix & its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## Write a short comment describing this function
## Is the data cached, return the cached value
## If not, return the results of solve() and set the cached results
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
