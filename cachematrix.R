## Function makeCacheMatrix
## stores matrix "x" and cache its inverse matrix "cacheInvM"
## Operators set and get do not check parameters
##
## operators
## set(x) store matrix "x"
## get() returns matrix
## setInvM(invM) cache inversed matrix
## getInvM() returns inverted matrix
## ! remember you get what you set (no cheking if matrix is inversed or not)

makeCacheMatrix <- function(x = matrix()) {
	## variablem with inverted matrix initially set to NULL
	cachedInvM <- NULL

	## setting matrix sets inverse matrix to NULL
	set <- function(y){
		x <<- y
		cachedInvM <<- NULL
	}
	## returns matrix
	get <- function() x
	
	## sets inversed matrix (no checks!!!)
	setInvM <- function(invM) cachedInvM <<- invM

	## returns inversed matrix
	getInvM <- function() cachedInvM

	list(set = set, get = get, setInvM = setInvM, getInvM = getInvM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	cacheInvM <- x$getInvM()

	## checks if cache with inverse matrix is set
	if(!is.null(cacheInvM)) {
		message("getting cached data")
		return(m)
	}
	## if chache is not set calculate inverse of matrix and set it,
	## next return calculation
	mtrx <- x$get()

	invM <- solve(mtrx)
	
	x$setInvM(invM)

	invM

}
