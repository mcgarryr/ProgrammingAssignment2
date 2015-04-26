## These functions are used to cache and compute the inverse of a matrix

## The makeCacheMatrix function creates a special matrix object that is able to 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
                
}


## The cacheSolve function computes the inverse of the special matrix calculated
## by the makeCacheMatricx function. If the inverse has already been calculated,
## and the matrix hasn't changed, cacheSolve retrieves the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
        
}
