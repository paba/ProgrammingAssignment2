## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of 'x'
## if the inverse is already computed it returns the cache value
## if it is not already in the cache then get the matrix and compute inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    ## check if the inverse is already in the cache
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## if the inverse is not in cache then compute the inverse
    data <- x$get()
    i <- solve(data, ...)
    message("computing inverse")
    x$setinverse(i)
    i
}
