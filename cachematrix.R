## This program set the value of the given matrix and find the
## inverse of the matrix. If inverse already exists then it returns
## the inverse else it returns the inverse for the given matrix.

##GIVEN: The matrix

##FUNCTION: It creates a list that has function which serves following
##          purposes.
##		1. It sets the value of matrix using "setMatrix" function
##		2. It gets the matrix using "getMatrix" function which will
##             be used by cacheSolve function to calulate Inverse of 
##             given Matrix.
##		3. It stores the inverse of Matrix computed by cacheSolve 
##		   function into the list using "setInverse" function.
##		4. It gets the inverse of already computed matrixes which
##		   will be used by cacheSolve function in order to check; if
##		   inverse already exists; return the same inverse else compute
## 		   the inverse for new matrix.


makeCacheMatrix <- function(x = matrix()) {
	 ## Sets initial inverse to null and "setMatrix" function sets
	 ## the new matrix to the list.
 		inverse <- NULL
        	setMatrix <- function(y) {
           			x <<- y
                		inverse <<- NULL
        	}
		getMatrix <- function() x

	 ## sets the inverse into list for further caching.
		setInverse <- function(solve) inverse <<- solve

	 ## It retrieves the inverse form the list.
		getInverse <- function() inverse

	 ## List containing all the information from which the inverse, matrix 
	 ## will be extracted and the new inverse and new matrix will be set into.
		list(setMatrix = setMatrix, getMatrix = getMatrix,
             	setInverse = setInverse, getInverse = getInverse)
}


##FUNCTION: This function serves two purposes:
##		1. It checks for the inverse in the list; If inverse exist it
##		   the same inverse that is the cached result
##		2. If inverse for the given matrix comes out to be null; then it 
##		   returns the computed inverse for it and set it into the list for
##		   further need.
		
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  ## The inverse from the list for the given matrix is retrieved.
		inverse <- x$getInverse()

	  ## It checks if the inverse is not null; that is it has been already
	  ## computed, the previous stored inverse is returned.
		if(!is.null(inverse)) {
            		message("Cached inverse of matrix is returned")
		          	return(inverse)
        	}

	  ## If the inverse comes out to be null; it gets the matrix form the list.
		matrix <- x$getMatrix()

	  ## It computes the inverse for the matrix.
		inverse <- solve(matrix, ...)

	  ## It sets the inverse for the matrix into the list
		x$setInverse(inverse)

	  ## It returns the computed inverse to be printed on console.
        	inverse

}
