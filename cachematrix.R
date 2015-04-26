## Below are two function to perform matrix inversion using caching. Function makeCacheMatrix
## creates a special matrix object which is able to cache its inverse, second function
## cacheSolve performs the actual operation of inverse calculation based on whether
## cache result is present or not and matrix has not been changed after caching.



## Function to compute the matrix object which is able to cache the inverse of 
## the matrix1. This function accepts a matrix as input, it is assumed this matrix
## is invertible as per the instructions for this assignment. This object also 
## maintains a boolean variable matrixChanged to check if matrix has been 
## altered post calculation of the inverse. If matrix is changed then cached 
## value is not considered and inverse is recomputed and stored in the cached inverse value.
	
makeCacheMatrix <- function(x = matrix()) {
	
	

	m <<- x
	matrixChanged <<- TRUE
	invM <<- NULL
	set <- function(y) {
		 m <<- y
		matrixChanged <<- TRUE
	}
	get <- function() m
	setInverse <- function(inv) {
		invM <<- inv
	}
	getInverse <- function() invM
	getMatrixChanged <- function() matrixChanged
	setMatrixChanged <- function(mSet) {
		matrixChanged<<-mSet
	}
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse,
		 getMatrixChanged = getMatrixChanged,
		 setMatrixChanged = setMatrixChanged
		 )

}


## Function to get inverse of a matrix object returned by makeCachedMatrix
## Returns a matrix containing the inverse of the input matrix

cacheSolve <- function(x, ...) {
	
	invM <- x$getInverse()
	mChanged <- x$getMatrixChanged()	
	
	## Check if value present in cache and matrix has not been changed.
	if(!is.null(invM) && mChanged==FALSE ){
		print("Getting cached inverse")
		return(invM)
	}
	mat <- x$get()
	invM <- solve(mat)
	x$setInverse(invM)
	x$setMatrixChanged(FALSE)
	invM
}
