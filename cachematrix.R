## The following functions are used to inverse of a Matrix and cache the result
## Example of use:
## > mat <- matrix(1:4,2,2)
## > cm <- makeCacheMatrix(mat)
## > inv <- cacheSolve(cm)

## The result of the following equation should be an identity matrix
## > mat %*% inv

## A second call to the cacheSolve method should display "getting cached data" as follow
## > inv <- cacheSolve(cm)
## getting cached data


## Creates a matrix used for caching
## Parameter x should be a square invertible matrix
makeCacheMatrix <- function(x = matrix()) {
	## Initialize the local variable
	inv <- NULL
	
	## Reset the initial matrix and reset the local variable inv
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	## Returns the initial matrix
	get <- function() x
	
	## Store the inverted matrix
	setinv <- function(inverse) inv <<- inverse
	
	## Returns the inverted matrix
	getinv <- function() inv
}

## Inverse a Matrix and cache it
## Parameter x should be an object created via the function makeCacheMatrix
cacheSolve <- function(x, ...) {
	## Check in the cache if the inverse has already been computed
	inv <- x$getinv()
	
	## When the inverse matrix exists in the cache, return it and post a message
	if (!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	
	## Get the matrix
	data <- x$get()
	
	## Compute the inverse
	inv <- solve(data)
	
	## Store the inverse in the cache
	x$setinv(inv)
	
	## Return the inverse matrix
	inv
}
