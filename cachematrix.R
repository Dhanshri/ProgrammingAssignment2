## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL
        # assigns matrix value
	set <- function(z=matrix()) {
                x <<- z
                mInv <<- NULL
        }
        # fetches matrix
	get <- function() x
	# sets Inverse of matrix to matrixInverse passed
        setInv <- function(Inverse) mInv <<- Inverse
        # gets Inverse of matrix if available,else returns NULL
        getInv <- function() mInv
        # function return list type
        list(set = set, get = get,
             setInv = setInv ,
             getInv = getInv )


}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInv <- x$getInv()
        # checks to see if Inverse is already calculated. If 'yes' it skips the calculation and returns the value from cache.
        if(!is.null(mInv)) {
                message("getting cached data")
                return(mInv)
        }
        data <- x$get()
        # calculates matrix Inverse
        mInv <- solve(data, ...)
        x$setInv(mInv)
        mInv

}
