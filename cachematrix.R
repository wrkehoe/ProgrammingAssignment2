
## Johns Hopkins/Coursera MOOC: R Programming

## Assignment 2: to write a pair of functions that cache the inverse
## of a matrix.

## makeCacheMatrix creates a special matrix object that includes its own
## getter and setter functions. They cache the matrix  and its
## inverse in the makeCacheMatrix function's environment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <-function(y){
        x <<- y
        m <<- NULL
     }

    get <- function() x
    setInverse <-function(inverseMatrix) m <<- inverseMatrix
    getInverse <-function() m

    list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)


}


## cacheSolve is passed a matrix of the special type created by
## makeCacheMatrix. It first checks the cache in the makeCacheMatrix
## environment; if an inverse is stored there, cacheSolve returns
## it. If not, it solves for the inverse matrix, stores it in the
## cache, then returns its

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }

    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
    m

}
