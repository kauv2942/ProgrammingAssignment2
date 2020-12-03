## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix consists of set, get, setinverse, & getinverse
## makeCacheMatrix is meant to create a special matrix that can cache its reverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL           # it initilizes the inverse as NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL 
    }
    get <- function() x     #function is used to get matrix x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv      #function is used to get inverse of matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

## function is used to get the cache of the data
cacheSolve <- function(x, ...) {        # this obtains the cache data
  inv <- x$getinverse()
    if (!is.null(inv)) {              # this checks if the inverse is NULL
        message("getting cached data")
        return(inv)                   # returns inverse value
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
