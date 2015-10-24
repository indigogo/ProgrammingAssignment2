# These functions can be used to cache matrix inverse operations

## makeCacheMatrix creates object that caches matrix and inverse and calcs if doesnt exist
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function( solve ) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
		
}


## cacheSolve asks for inverse val of above object if exists or asks makeCacheMatrix to solve it
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv

}
