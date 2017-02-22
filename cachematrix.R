## makeCacheMatrix and cacheSolve enable inversion of the input matrix
## and caching of the result, so that when the same matrix is input to
## cacheSolve another time, the cached output is retrieved, instead of 
## having to perform the calculation again

## function: makeCacheMatrix
## arguments: matrix
## output: a list of functions that:
## 1) return value of matrix (getm)
## 2) set value of matrix inverse (setsolve)
## 3) return value of matrix inverse (getsolve)

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	setm <- function(y) {
		x <<- y
		i <<- NULL
		}
	getm <- function() x	
	setsolve <- function(inv) i <<- inv
	getsolve <- function() i
	
	list(setm = setm, getm = getm, setsolve = setsolve, getsolve = getsolve)
}

## function: cacheSolve
## arguments: list returned by makeCacheMatrix
## output: inverted matrix, either calculated anew or retrieved from cache
##	(accompanied by a notice)
## if inverted matrix is created anew, it is passed back to back to
##	makeCacheMatrix (as input to setsolve)

cacheSolve <- function(x, ...) {
	i<-x$getsolve()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
		}
	data<-x$getm()
	i<-solve(data, ...)
	x$setsolve(i)
	i
}
