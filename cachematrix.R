## These functions will store (set) and return (get) a square matrix
## They will also store (setinv) and return (getinv) the inverse of same

## makeCacheMatrix gets and sets the value of the matrix and its inverse
## it needs these values provided to it for caching

makeCacheMatrix <- function(x = matrix()) {
    #set the value of the matrix & inverse
    #get the value of the matrix & inverse
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(t) m <<- t
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## CacheSolve will solve for the inverse of a matrix when called initially
## and will store it using the internal setinv function of the x object
## once initially stored cacheSolve will return a cached version versus recalc

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
