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
        setmean <- function(new_inv) inverse <<- new_inv
        
        ## return the "stored" inverse
        getmean <- function() inverse

        ## above functions are provided trough a list
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


## Calculate the inverse of the special "cached matrix" as created
## by the function above. If we can read the value from the cache,
## do this instead of calculating again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
        
}
