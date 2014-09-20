## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    #It is an object that contains a matrix and its inverse, the
    #latter once it's calculated by using the next function, otherwise
    # it is has NULL value
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    #Returns the inverse of the matrix stored in x,
    #x being a "special" matrix created with makeCacheMatrix.
    #If the inverse is not previously calculated the function
    #gets it using solve and stores it in x "permanently".
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
