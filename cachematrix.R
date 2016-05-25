## Put comments here that give an overall description of what your
## functions do

## This function creates a special vector that caches the calculation of its inverse
## It does this by creating the object as a list with `set` and `get` functions,
## and some clever scoping magic from R
makeCacheMatrix <- function(x = matrix()) {
	## from start ,set the inv with the null
	inv <- NULL

	## assiging the new argument to the stored value
	## at the again set the null to inv
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	## return the internal object
	get <- function() x
	## set and get the function for the inverse
	setInverse <- function(inverse) inv <<- inverse
  	getInverse <- function() inv
  	list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)


}


## This function will calculate the inverse of the received square matrix, caching
## the result in the CacheMatrix object.
## In the cache already exists, it will simply return the cached result
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if(!is.null(inv)) {
	message("getting cached data")
	return(inv)
	}
	data <- x$get()
	inv <- solve(data)  ## Calculating the inverse
	x$setInverse(inv)
	inv
}
