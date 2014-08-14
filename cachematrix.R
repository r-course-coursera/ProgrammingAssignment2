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

cacheSolve <- function(matrixWithCache, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## try to read cached inverse
        cached_inverse <- matrixWithCache$getinverse()
                
        ## cached inverse found => return it
        if(!is.null(cached_inverse)) {
                message("getting cached data")
                return(cached_inverse)
        }
        
        ## cache inverse not found => calculate it
        
        ## get "real" matrix from the "cached matrix" input parameter
        data <- matrixWithCache$get()
        
        ## calculate inverse by using the solve() function
        
        ## (note that solve() only works on square invertible matrices
        ## and while the assignment's description tells us to assume the
        ## matrix to be inversible, it does not actually tell us that the
        ## matrix is square. However, we'll assume it is, here.)
        inverse <- solve(data, ...)
        
        # "store" the inverse for this matrix
        matrixWithCache$setinverse(inverse)
        
        # return the calculated inverse
        inverse
}
