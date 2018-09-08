## The following 2 functions make the computer to check if it has done calculation of 
## the inverse of a matrix before. If yes, then the computer doesn't have to re-calculate 
## again, it can retrieve from cache memory. 

## The function makeCacheMatrix creates a list of special functions, so that the computation 
## of the inverse of a matrix can be stored in cache. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## The function cacheSolve will return the inverse of a matrix. The matrix should be 
## pre-processed with the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- Inverse(data, ...)
        x$setInverse(i)
        i
}
