# makeCacheMatrix and cacheSolve enable inversion of the input matrix
# and caching of the result, so that when the same matrix is input to
# cacheSolve another time, the cached output is retrieved, instead of 
# having to perform the calculation again

# function: makeCacheMatrix
# arguments: matrix
# output: a list of functions that:
# 1) return value of matrix (getm)
# 2) set value of matrix inverse (setsolve)
# 3) return value of matrix inverse (getsolve)

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL

	getm <- function(){x}	
	setsolve <- function(y){im <<- y}
	getsolve <- function(){im}
	
	list(getm = getm, setsolve = setsolve, getsolve = getsolve)
}

# function: cacheSolve
# arguments: list returned by makeCacheMatrix
# output: inverted matrix, either calculated anew or retrieved from cache
#	(accompanied by a notice)
# if inverted matrix is created anew, it is passed back to back to
#	makeCacheMatrix (as input to setsolve)

cacheSolve <- function(x, ...) {

	if(!is.null(x$getsolve())) {
		message("getting cached data")
		return(x$getsolve())
		}

	x$setsolve(solve(x$getm()))
	return(solve(x$getm()))
}
