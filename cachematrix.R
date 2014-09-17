## makeCacheMatrix creates an object that holds a matrix (x) and its inverse (mi). 
## The object has getter and setter for original matrix (get, set) and for the inverse (getinverse, setinverse)

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setinverse <- function(invr) mi <<- invr
    getinverse <- function() mi
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve receives x, an object created by makeCacheMatrix and if the inverse of the matrix stored in x
## was calculated before, the inverse matrix(mi) would be returned, otherwise the inverse(mi) is calculated, stored in x,
## then returned to the caller

cacheSolve <- function(x, ...) {
    mi <- x$getinverse()
    if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
    }
    data <- x$get()
    mi <- solve(data, ...)
    x$setinverse(mi)
    mi
}