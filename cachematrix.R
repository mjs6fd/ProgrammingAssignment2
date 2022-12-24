## The first function creates a sspecial vector of functions that act as getters
## and setters for a given invertible matrix, as well as its inverse if it's 
## already been calculated.

## The second function uses the object created from the first function as an input
## to calculate the inverse of a matrix. If the inverse was already calculated in a 
## prior call, and the matrix hasn't changed, then the function will return the cached
## inverse matrix instead of going through the computation from scratch again.



## Creates a list of getter/setter functions for a matrix and its calculated inverse.
## Setting the inverse caches the value in a variable that can be retrieved in a
## different R environment, so that if the matrix hasn't changed, the inverse doesn't
## need to be recalculated.
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y) x <<- y
        
        get <- function() x
        
        setinverse <- function(inv_x) m <<- inv_x
        
        getinverse <- function() m
        
        list(set = set
             , get = get
             , setinverse = setinverse 
             , getinverse = getinverse
             )

}


## Takes a list of functions created from makeCacheMatrix as input, which is used
## to store getters and setters for an invertible matrix and its inverse.
## If the inverse has already been calculated for a matrix, and the matrix hasn't
## changed, the function will retrieve it without being calculated.
## Otherwise, it will calculate the inverse and cache it in the list object being 
## used as the input for the function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        matrix_init <- x$get()
        m <- solve(matrix_init, ...)
        x$setinverse(m)
        m
}
