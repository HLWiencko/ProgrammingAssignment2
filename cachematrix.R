## A pair of functions that finds the inverse of a matrix and caches it,
## checking first to see if there's already cached data. 
##
## Call it like this:
## my_inverse <- cacheSolve(makeCacheMatrix(my_matrix))

## Creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse, 
         getInverse = getInverse)
}

## Gets the inverse of the "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated and the matrix has
## not changed, retrieve the cached inverse. 

cacheSolve <- function(x, ...) {
    
    # Do we have an inverse already?
    i <- x$getInverse()
    if(!is.null(i)) {  
        message("Getting cached data")
        return(i)
    }
    
    # No inverse cached, so we calculate it here.  
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    return(i)   
}
