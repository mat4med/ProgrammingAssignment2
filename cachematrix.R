## The following are a pair of functions that cache the inverse of 
## a matrix.
## makeCacheMatrix: creates a special "matrix"; and
## cacheSolve: computes the inverse of the special "matrix" or
## retrieve the inverse from the cache.


## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setSolve <- function(slv) s <<- slv
	getSolve <- function() s
	list(set = set, get = get,
		setSolve = setSolve, getSolve = getSolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	s <- x$getSolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setSolve(s)
	s
}


## How to use makeCacheMatrix and cacheSolve functions
## > a <- matrix(1:4, 2, 2)
## > a
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## > b <- makeCacheMatrix(a)
## > cacheSolve(b)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## > cacheSolve(b)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5



















