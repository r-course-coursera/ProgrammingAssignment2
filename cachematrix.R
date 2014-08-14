## Provide a function that calculates the inverse of a matrix using a cache
## so that subsequent calls on one and the same matrix are answered
## from the cache instead of calculating them again.


## This is a "cached matrix" which is implemented as a list
## that provides means to
## - set a matrix
## - get a matrix
## - set the inverse of a matrix
## - get the inverse of a matrix

makeCacheMatrix <- function(matrix = matrix()) {
        ## initialize "empty" inverse
        inverse <- NULL
        
        ## "store" the input matrix; the inverse is "empty"
        set <- function(new_matrix) {
                matrix <<- new_matrix
                inverse <<- NULL
        }
        
        ## return the "stored" matrix
        get <- function() matrix
        
        ## "store" the inverse
        setinverse <- function(new_inverse) inverse <<- new_inverse
        
        ## return the "stored" inverse
        getinverse <- function() inverse

        ## above functions are provided trough a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculate the inverse of the special "cached matrix" as created
## by the function above. If we can read the value from the cache,
## do this instead of calculating again.

cacheSolve <- function(cachedmatrix, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- cachedmatrix$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- cachedmatrix$get()
        inverse <- mean(data, ...)
        cachedmatrix$setinverse(inverse)
        inverse
        
}
