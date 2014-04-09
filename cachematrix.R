## Put comments here that give an overall description of what your
## functions do

##  makeCacheMatrix
#
#   Creates an object for a (square!) matrix to prepare for caching its inverse
#   containing a getter and setter (returning the value set) 
#   for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- matrix()
    set <- function(y) {
        i <<- matrix()
        x <<- y
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

##  cacheSolve
#
#   returns the inverse of a (square!) matrix's caching object
#   if no inverse exists yet will store it will determine it an via its setter
#   or return the cached value otherwise
#

cacheSolve <- function(x, ...) {
    emptyMatrix <- function (i) {
        #   an "empty" matrix has dimension 1 by 1 and its only value is NA
        (identical(dim(i), as.integer(c(1, 1))) && is.na(i[1,1])) 
    }
    
    i <- x$getinv()
    if(!emptyMatrix(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
}
