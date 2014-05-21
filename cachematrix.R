## cachematrix.R
## This file contains two functions for calculating and returning c cached inverse of a matrix

## makeCacheMatrix
## This function creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve
## This function uses a cached matrix to calculate a matrix inversion
## if the inverse has already been calculated, the cached values are returned
cacheSolve <- function(x, ...) 
{

    # get the cahced results
    inv <- x$getinv()
    if(!is.null(inv)) 
    {
        message("getting cached data")
        return(inv)
    }
    
    # if no cached results are retrieved, calculate the inversion and return that result
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    return(inv)
}
