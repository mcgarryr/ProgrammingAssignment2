## These functions are used to cache and compute the inverse of a matrix

## The makeCacheMatrix function creates a special matrix object that is able to 
## cache its inverse

makeCacheMatrix <- function(inputMatrix = matrix()) {
        
        cachedMatrix <- NULL
        
        #Reset cached inverse
        
        set <- function(y) {
                inputMatrix <<- y
                cachedMatrix <<- NULL
        }
                
        get <- function() inputMatrix
        
        setSolved <- function(solve) cachedMatrix <<- solve
        getSolved <- function() cachedMatrix
        
        list(set = set, 
             get = get,
             setSolved = setSolved,
             getSolved = getSolved)
                
}


## The cacheSolve function computes the inverse of the special matrix calculated
## by the makeCacheMatricx function. If the inverse has already been calculated,
## and the matrix hasn't changed, cacheSolve retrieves the inverse.

cacheSolve <- function(inputMatrix, ...) {
        
## Return a matrix that is the inverse of the special matrix cached by 
## makeCachedMatrix function
        
        solved <- x$getSolved()
        
## Check for existing inverse matrix, return it if it exists
        
        if(!is.null(cachedMatrix)) {
                message("Getting cached inverted matrix...")
                return(cachedMatrix)
        }
        
        else {
        targetMatrix <- inputMatrix$get()
        solved <- solve(targetMatrix, ...)
        inputMatrix$setInverted(solved)
}        
solved
        
}
