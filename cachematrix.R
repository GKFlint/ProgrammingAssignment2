## The following are a pair of functions to create, calculate and cache matrix inversions
## so that one does not need to calculate it repeatedly.

## makeCacheMatrix
## This function creates a special matrix object along with a few subfunctions to operate on it, in
## order to cache its inverse. Note the use of the "<<-" or super assignment used so that values
## in the parent environment can be accessed. Additionally, whenever the object's matrix is changed
## its cache is reset to NULL, so that we can recycle the object and also not accidentally
## end up with the wrong inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
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


## cacheSolve
## This function computes the inverse of the matrix object created by makeCacheMatrix, returning
## the inverse if it has already been calculated for the particular matrix received.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}