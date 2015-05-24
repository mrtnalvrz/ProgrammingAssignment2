## Put comments here that give an overall description of what your
## functions do

## 1.	makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## By Jose M Alvarez
## Last modification May 23, 2015

makeCacheMatrix <- function(x = matrix()) {
 

	## set the value of the matrix
	## get the value of the matrix
	## set the value of the inverse matrix
	## get the value of the inverse matrix
        
    # The <<- operator can be used to assign a value to an object in an environment that is different from the current environment.
		
	inverseMatrix = NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
		

	get <- function() x
    setInverse <- function(solve) inverseMatrix <<- solve 
    getInverse <- function() inverseMatrix
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## 2.	cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.## 1.	makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## By Jose M Alvarez
## Last modification May 23, 2015

cacheSolve <- function(x, ...) {

	## Return a matrix that is the inverse of 'x'
        
    inverseMatrix <- x$getInverse()
        
    # Verify if the inverse matrix has already been generated
	if (!is.null(inverseMatrix)){
		message("getting cached data")
        return(inverseMatrix)
	}
        
    # Calculate the inverse Matrix for the first time - Not cached
    matrix <- x$get()
    inverseMatrix <- solve(matrix, ...)
        
    # Stores the value of the inverse matrix in the cache
    x$setInverse(inverseMatrix)
    return(inverseMatrix)
}