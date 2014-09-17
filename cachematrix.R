## R programming Assignment 2
## Liina Abner
## This function creates a special "matrix" object that can cache its inverse
## No error handling in this code

## makeCacheMatrix takes matrix as argument and returns a list of functions.
## In set and set_in function values are assigned to variables with <<- operator,
## making them accessible from environment enclosing the function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL 
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() {
		x
	}
	set_in <- function(i) {
		inv <<- i
	}
	get_in <- function() {
		inv
	}
	list(set=set,get=get,set_in=set_in,get_in=get_in)
}


## calculates inverse of the "special" matrix created by makeCacheMatrix function.
## it first checks if inverse has already been calculated by checking the value of inv
## variable stored by makeCacheMatrix. If it is not null the value is returned
## otherwise the value is calculated with solve() function and stored by set_in() function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$get_in()
	if(!is.null(inv)) {
		return(inv)
	}
	mtrx <- x$get()
	inv <- solve(mtrx)
	x$set_in(inv)
	inv
}
