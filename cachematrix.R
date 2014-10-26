## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Get/set functions for matrix

makeCacheMatrix <- function(x = matrix()) {
	# initial inverse to NULL
	i <- NULL

	# set matrix value and reset inverse to NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	# get matrix value	
	get <- function() x

	# set inverse value
	setinverse <- function(inverse) i <<- inverse

	# get inverse value
	getinverse <- function() i

	# return a list of functions than can be called
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## Write a short comment describing this function
# Get inverse of a matrix. If cached, return cached value;
# otherwise, compute inverse and cache it.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()

	# if cached value is valid, return it
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	# if no cached value
	# get matrix value
	data <- x$get()

	# compute inverse
	i <- solve(data, ...)

	# cache inverse
	x$setinverse(i)

	# return inverse
	i
}
