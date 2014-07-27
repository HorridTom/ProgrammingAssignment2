## This script defines two functions that together provide functionality to
## store and retrieve a matrix, calculate it's inverse, and cache that
## inverse to be retrieved again later when needed. This is useful because
## matrix inversion is relatively costly. 

## The function makeCacheMatrix provides the basic storing and retrieval
## functionality described above, for both the matrix itself, and its inverse

makeCacheMatrix <- function(x = matrix()) {
	I <- NULL
	set <- function(y) {
		x <<- y
		I <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) I <<- inverse
	getinverse <- function() I
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## The cacheSolve function takes as argument a list created by the
## makeCacheMatrix function, and returns the inverse of the associated
## matrix, by either i) returning the cached inverse calculated
## previously, or ii) calculating the inverse and caching it for future
## use.

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'

	I <- x$getinverse()
	if(!is.null(I)) {
		message("getting cached data")
		return(I)        
	}
	mat <- x$get()
	I <- solve(mat, ...)
	x$setinverse(I)
	I

}
