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
## 		Sets initial inverse to null and "setMatrix" function sets
## 		 the new matrix to the list.
 
		inverse <- NULL
        	setMatrix <- function(y) {
           			x <<- y
                		inverse <<- NULL
        	}
		getMatrix <- function() x
##--------->sets the inverse into list for further caching.
		setInverse <- function(solve) inverse <<- solve
##--------->It retrieves the inverse 
        	getInverse <- function() inverse
        	list(setMatrix = setMatrix, getMatrix = getMatrix,
             	setInverse = setInverse, getInverse = getInverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 		inverse <- x$getInverse()
        	if(!is.null(inverse)) {
            		message("Cached inverse of matrix is returned")
		          	return(inverse)
        	}
        	data <- x$getData()
	      inverse <- solve(data, ...)
        	x$setInverse(inverse)
        	inverse

}
