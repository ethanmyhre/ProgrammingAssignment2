## functions to cache an inverted matrix in order to have the value readily
## availble in order to prevent having to solve it every time it's needed

## makeCacheMatrix contains functions for getting and setting the matrix
## and getting and setting the inverse.  returns a list

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
   
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setSolved <- function(solved) m <<- solved
    
    getSolved <- function() m
    
    list(set = set, get = get, setSolved = setSolved,
         getSolved = getSolved)
    
}


## cacheSolve returns the cached inverse matrix if it has already been
## solved, or it solves and caches the matrix before returning it

cacheSolve <- function(x, ...) {
    
    m <- x$getSolved()
    
    if(!is.null(m)){
        message("getting cached inverse matrix")
        return(m)
    }
    
    data <- x$get()
    
    m <- solve(data,...)
    x$setSolved(m)
    m
    
}
