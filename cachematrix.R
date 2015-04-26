## These functions are used to cache and compute the inverse of a matrix

## The makeCacheMatrix function creates a special matrix object that is able to 
## cache its inverse

makeCacheMatrix <- function(inputMatrix = matrix()) {
        
        
        
        invertedMatrix <- NULL
        
        set <- function(y) {
                inputMatrix <<- y
                invertedMatrix <<- NULL
        }
        
        get <- function() inputMatrix
        
        setInverted <- function(solve) invertedMatrix <<- solve
        getInverted <- function() invertedMatrix
        
        list(set = set, get = get,
             setInverted = setInverted,
             getInverted = getInverted)
                
}


## The cacheSolve function computes the inverse of the special matrix calculated
## by the makeCacheMatricx function. If the inverse has already been calculated,
## and the matrix hasn't changed, cacheSolve retrieves the inverse.

cacheSolve <- function(inputCached, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invertedMatrix <- inputCached$getInverted()
        if(!is.null(invertedMatrix)) {
                message("Getting cached inverted matrix")
                return(invertedMatrix)
        }
        
        targetMatrix <- inputCached$get()
        invertedMatrix <- solve(targetMatrix)
        inputCached$setInverted(invertedMatrix)
        invertedMatrix
        
}
