

## The following function will create a square invertible matrix using makeCacheMatrix function
##and make the inverse of the matrix available in the cache environment using cachesolve function


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = numeric()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function()
        x
    setinverse <- function(inverse)
        inv <<- inverse
    getinverse <- function()
        inv
    list(
        set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    ## If cache is not null,skip inverse function and return cache data for the given function call
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix_data <- x$get()
    ## using solve function to inverse the matrix.
    s <- solve(matrix_data, ...)
    x$setinverse(s)
    s
}
