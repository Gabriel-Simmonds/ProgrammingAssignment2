## These two functions take a matrix, and then via a list obtain the inverse 
## either through calculation or by obtaining previously stored data in the 
## cache.

## This first function will create a special "matrix", which is a list 
## containing a function to:-
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse matrix using the solve() function
## 4) get the value of the inverse matrix using the solve() function
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setSolve <- function(solve) s <<- solve
	getSolve <- function() s
	list(set = set, get = get,
		setSolve = setSolve,
		getSolve = getSolve)
}


## This second function calculates the inverse of the special "matrix" created 
## with the makeCacheMatrix function. First it checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and no calculation is necessary. Otherwise, it calculates the inverse 
## of the matrix and assigns the value to the cache via the setSolve function.
cacheSolve <- function(x, ...) {
	s <- x$getSolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setSolve(s)
	s
	## Return a matrix that is the inverse of 'x'
}