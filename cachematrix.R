## makeCacheMatrix is passed a matrix and it returns a list containing the following functions:
##
##  setMatrix
##  getMatrix
##  setInverse
##  getInverse
##
## when this function is initally invoked the input matrix is saved in mtx parameter

makeCacheMatrix <- function(mtx = matrix()) {
  
  ## initialize inverse
  invmtx <- NULL 
  
  ## set Matrix
  setMatrix <- function(y) {
    mtx <<- y
    invmtx <<- NULL
  }
  
  ## get Matrix
  getMatrix <- function() mtx

  ## set Inverse
  setInverse <- function(z) invmtx <<- z
  
  ## get inverse
  getInverse <- function() invmtx
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve is passed a list parameter that has been created with makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## the function tries to retrieve the inverse of the matrix from the input list
  i <- x$getInverse()
  
  ## is the inverse is found, it is returned (instead of being recalculated)
  if(!is.null(i)) {
    message("getting cached data")
    return(i) ## the function is exited returning the inverse
  }
  
  ## if the inverse is not found, the function continues processing.
  
  data <- x$getMatrix() ## the matrix from the input list is passed to the 'data' object
  i <- solve(data, ...) ## the inverse of 'data' is calculated using the function 'solve'
  x$setInverse(i) ## the inverse is saved to the input list
  i ## the inverse is returned
}


