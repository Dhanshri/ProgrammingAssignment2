## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL
	set <- function(z=matrix()) {
                x <<- z
                mInv <<- NULL
        }
	get <- function() x
        setInv <- function(Inverse) mInv <<- Inverse
        getInv <- function() mInv
        list(set = set, get = get,
             setInv = setInv ,
             getInv = getInv )


}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         mInv <- x$getInv()
        if(!is.null(mInv)) {
                message("getting cached data")
                return(mInv)
        }
        data <- x$get()
        mInv <- solve(data, ...)
        x$setInv(mInv)
        mInv

}
