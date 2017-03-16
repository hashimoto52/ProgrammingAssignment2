## R Programming Week2 Assignment
##
## makeCacheMatrix creats a list that store a matrix (x)
## and its inverse. 
## cacheSolve returns x's inverse stored in the list
## that created by makeCacheMatrix.

## makeCacheMatrix makes a list contains x (x is a matrix)
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) inv <<- solve
        getSolve <- function() inv
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)

}


## cacheSolve returns inverse from a list that
## makeCacheMatrix make.

cacheSolve <- function(x, ...) {
        inv <- x$getSolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setSolve(inv)
        inv
}
