## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

r2
## The function cacheSolve first tries to retrieve

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$getMatrix()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}


