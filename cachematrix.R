## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix takes a matrix as input and exposes a getter and setter for the original matrix as well as the inverse.

#after passing in a matrix, you are able to call getInverse
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getMatrix <- function() x
  
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
#cacheSolve will return an inverse matrix that has been cached in makeCacheMatrix or caculate the inverse of the matrix and return it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  
  if( !is.null(inverseMatrix) ) {
    print("returning cached inverse matrix.")
    return(inverseMatrix)
  } else {
    data <- x$getMatrix()
    inverseMatrix <- solve(data, ...)
    x$setInverse(inverseMatrix)
    return(inverseMatrix)
  }
}


