## Put comments here that give an overall description of what your
## functions do

## The "makeCacheMatrix" function resets the variable i in current environment to NULL, defines the following 4 functions and returns a named list containing these functions:
    ## set = store a matrix in the variable x and reset the variable i to     Null in the parent environment
    ## get = return the x, which is cached in the environment of "makeCacheM    atrix"
    ## setinverse = store a inverse in the variable i in the parent environment
    ## getinverse = return the i, which is cached in the environment of "makeCacheMatrix"

makeCacheMatrix <- function(x = matrix()) {
    i  <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The "cacheSolve" function returns the cached inverse of a matrix if already calculated, otherwise calculate the inverse of the matrix and cache it in the environment of "makeCacheMatrix"

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
