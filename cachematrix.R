## This pair of functions cache the inverse of a matrix. If the contents of a matrix are not changing,
## they cache the inverse matrix so that when it is needed again, it can be looked up in the cache 
## rather than recomputed. 

## Function makeCacheMatrix() creates a special "matrix" object that can cache its inverse and which is
## really a list containing a function to 1) set the valua of the matrix, 2) get the value of the matrix,
## 3) set the value of the inverse matrix, 4) get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## Return a list of the four funcions
        i <- NULL                              ## sets the cache to NULL
        set <- function(y) {
                x <<- y                        ## takes the matrix and assigns it to x in the parent environment
                i <<- NULL                     ## cleanes the cache in the parent environment
        }
        get <- function() x                    ## retrieves the matrix
        setinv <- function(solve) i <<- solve  ## puts the inverse matrix into the parent environment
        getinv <- function() i                 ## retrieves the inverse matrix
        list(set = set, get = get,             ## returns the list of the four functions
             setinv = setinv,
             getinv = getinv)
}


## Funcion cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()                        ## gets the inverse matrix from the cache
        if(!is.null(i)) {                      ## checks if the inverse matrix has been calculated
                message("getting cached data")
                return(i)                      ## gets the inverse matrix from the cache and skips computation
        }
        ## Otherwise...
        data <- x$get()                        ## retrieves the matrix
        i <- solve(data, ...)                  ## calculates the inverse of the matrix
        x$setinv(i)                            ## sets the value of the inverse matrix in the cache
        i                                      ## returns the inverse of the matrix
}
