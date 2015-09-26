## This file provides functions to store the inverse of a matrix 
## in memory and retrieve it when needed without having to call the 
## solve function which can be a costly opereation.


## Function makeCacheMatrix takes an invertible matrix 
## and returns a list that provides functions that
##
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

    ## variable to hold inverse matrix
    m <- NULL
    
    ## function to set the matrix
    ## clears out the stored inverse matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## function to return the matrix
    get <- function() x
    
    ## function to set the inverse matrix
    setInverse <- function(inverse) m <<- inverse
    
    ## function to return the inverse matrix
    getInverse <- function() m
    
    ## return a list of the  specified functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}

## Function cacheSolve returns a matrix that is the inverse of 
## the passed in matrix the passed in matrix is  the list 
## returned from the makeChacheMatrix function. The function 
## returns a cached version if it exists, or sets it if it doesn't
cacheSolve <- function(x, ...) {

    ## get the chanced version
    m <- x$getInverse()
    
    ## check if a value was returned
    if(!is.null(m)) {
        
        ## if so return the cached value
        message("getting cached data")
        return(m)
    }
    
    ## no cached version so get the non inverse matrix 
    data <- x$get()
    
    ## get the inverse value
    m <- solve(data, ...)
    
    ## set the inverse value
    x$setInverse(m)
    
    ## return the inverse
    m
    
}
