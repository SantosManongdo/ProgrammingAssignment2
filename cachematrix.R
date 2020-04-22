## Caching the inverse of a matrix
## As matrix inversion is usually a costly computation, getting the inverse of a matrix and then caching it rather than
## compute it repeatedly will save time. 

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
   invmat <- NULL
    set <- function(y) {
            x <<- y
            invmat <<- NULL
    }
    get <- function() x
    setinvmat <- function(solve) invmat <<- solve
    getinvmat <- function() invmat
    list(set = set, get = get,
            setinvmat = setinvmat,
            getinvmat = getinvmat) 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- x$getinvmat()
        if(!is.null(invmat)) {
                message("getting cached inverse matrix")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        x$setinvmat(invmat)
        invmat
        
}
