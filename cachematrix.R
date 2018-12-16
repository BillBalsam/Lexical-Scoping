### This first function creates a matrix object that is able to cache it's own inverse

makeCacheMatrix <- function(x = matrix()) {
	inver <- NULL
	mcm <- function(y) {
		x <<-y
		inver<<-NULL
	}
	hold <-function() x
	setinver <- function(inverse) inver <<-inverse
	getinver <- function() inver
	list(mcm = mcm,
	     hold = hold,
	     setinver = setinver,
	     getinver = getinver)
}

### This function calculates the inverse of the latter matrix in makeCacheMatrix
### If there is no inverse to be calculated and the matrix has not been changed, then it will retrieve
### The inverse from the cache.

cacheSolve <- function(x, ...) {

	inver <- x$getinver()
	if (!is.null(inver)) {
		message("getting cached data")
		return(inver)
		}
		getting <-x$get()
		inver <- solve(getting,...)
		x$setinver(inver)
		inver
}
