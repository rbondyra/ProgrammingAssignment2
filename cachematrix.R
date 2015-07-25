## Matrix inversion is usually a costly computation. If the contents of
## a matrix are not changing, it may make sense to cache the value of the
## inverse so that when we need it again, it can be looked up in the cache
## rather than recomputed. 



## makeCacheMatrix -  this function creates a special "matrix" object that can
## cache its inverse. This function returns a list containing a function to: 
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the inverse matrix
## 4 - get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		
		set <- function(y){
				x <<- y
				m <<- NULL
		}		
		get <- function() x
		setSolve <- function(solved) m <<- solved
		getSolve <- function() m
		list(set = set, get = get,
			 setSolve = setSolve,
			 getSolve = getSolve)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. To do this, it uses R 'solve' function dedicated to
## such operations. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the data and sets
## the value of the inversed matrix in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
		m <- x$getSolve()
		if(!is.null(m)) {
				message("getting cached data")
				return(m)
        }
		data <- x$get()
		m <- solve(data, ...)
		x$setSolve(m)
		m  ## Return a matrix that is the inverse of 'x'    	
}
