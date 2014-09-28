## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function allows to create a matrix, populating and retrieving the values for the given matrix. 

## x = matrix() creates a matrix variable that is available in the makeCacheMatrix Context and not in the Global Context

makeCacheMatrix <- function(x = matrix())
{
        ## matrixCache is the cache name. holds the cache value.
        matrixCache <- NULL
        
        ## Function stores the give nmatrix.
        setMatrix <- function(newMatrixValue) 
        {
                x <<- newMatrixValue
                ## Clear the cache since a new matrix value has been assigned.
                matrixCache <<- NULL
        }
        
        ## retrieve the matrix that has been stored
        getMatrix <- function() x
        
        ## cache the given matrix
        setInverse <- function(solve) matrixCache <<- solve
        
        ## retrieve the matrix
        getInverse <- function() matrixCache
        
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## Write a short comment describing this function
## Inverse the matrix created by the last function and return the same
cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
        
        ## return the cached value if it exists
        if(!is.null(x$getInverse()))
                return (x$getInverse())
        
        ## Given the matrix, calculate its inverse 
        inverse <- solve(x$getMatrix())
        
        ##  cache the inverse
        x$setInverse(inverse)
        
        inverse
}
