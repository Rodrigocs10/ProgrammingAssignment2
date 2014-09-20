## These functions compute de inverse of a matrix and cache the
## result for future access

## This function caches a matrix with its inverse

makeCacheMatrix <- function(x = matrix()) {
	 inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function compute the inverse of a matrix
## if its inverse is not yet present in makeCacheMatrix
## or return the already present value in it
## Ex.: 
##		cache <- makeCacheMatrix(matrix(1:4, 2, 2))
##		cacheSolve(cache)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
			inverse <-x$getinverse()
			if(!is.null(inverse)){
				message("Getting cached inverse")
				return(inverse)
			}
			data <-x$get()
			inverse <- solve(data, ...)
			x$setinverse(inverse)
			inverse
}
			
