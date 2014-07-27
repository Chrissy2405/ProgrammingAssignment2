## Course: Coursera's R Programming course
## Description: Programming assignment 2
## Date: 27 July 2014

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <-x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}