## The functions within this solution will do the following:
## (1) Check to see if the inverse of a specific matrix has already been calculated.  If so then
##     it will return the already cached inverted matrix.
## (2) If the inverse has not already been calculated, then it will calculate the matrix, cache it,
##     and then return it.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## This function is called by the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  get <- function() x
  
  setInvMatrix <- function(im1) invMatrix <<- im1
  
  getInvMatrix <- function() invMatrix
  
  list(set=set, get=get,
       setInvMatrix=setInvMatrix,
       getInvMatrix=getInvMatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  invMatrix <- x$getInvMatrix()
  
  if (!is.null(invMatrix)){
    message("getting cached data")
    return(invMatrix)
  }
  
  origMatrix <- x$get()
  invMatrix <- solve(origMatrix, ...)
  
  x$setInvMatrix(invMatrix)
  
  return(invMatrix)
}
