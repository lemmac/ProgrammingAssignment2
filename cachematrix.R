## Function makeCacheMatrix
## Creates list of functions which operates on 2 internal variables
## Is used with function cacheSolve as cache for inverse matrix.
## Function argument can be matrix
## !you get what you set = no cheking if it is matrix or is inversed
## Functions names are misleading, but are created for function cacheSolve()

## Internal functions from list
## set(x) store variable "x" and cleans cachedInvM
## get() returns variable x
## setInvM(invM) set variable cachedInVM
## getInvM() returns variable cachedInVM


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


## Function cacheSolve inverses and caches matrix stored in variable x
## x have to be created with function makeCacheMatrix()
## Function checks if inversed matrix exist in variable x 
## Next if not exists calculates it set value in x and returns it
## If exists just return it without calculations


cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	cacheInvM <- x$getInvM()

	## checks if cache with inverse matrix is set
	if(!is.null(cacheInvM)) {
		message("getting cached data")
		return(cacheInvM)
	}
	## if chache is not set calculate inverse of matrix and set it,
	## next return calculation
	mtrx <- x$get()

	invM <- solve(mtrx)
	
	x$setInvM(invM)

	invM

}
