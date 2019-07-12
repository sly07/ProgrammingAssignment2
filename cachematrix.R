## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


