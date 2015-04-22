## The following functions compute inversion of a given matrix 
## We have cache with current matrix and its inversion (if already calculated)
## When trying to calculate, we check if it already have been calculated and stired in cache
## If so we just get it from cache,otherwise calculate and out in cache for next times


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
            x <<- y
            inverse <<- NULL
        }
        get <- function() x
        setInv <- function(solve) inverse <<- solve
        getInv <- function() inverse
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInv()
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        message("computing inverse matrix")
        inverse <- solve(x$get())
        x$setInv(inverse)
        inverse
}
