makeCacheMatrix <- function(x = matrix()) {
##Initial value of the inverse matrix
  matrixinverse <- NULL
##Setting the matrix  
  set <- function(y=matrix()) {
    x <<- y
##Change the value of inverse of the matrix in case the matrix was changed.
    matrixinverse <<- NULL  
  }
##Reading the matrix 'x' in the cacheSolve in case the inverse matrix hasn't computed yet.
  get <- function() x
##Computing the inverse matrix  
  setinverse <- function(solve) matrixinverse <<- solve
##Reading the inverse matrix  
  getinverse <- function() matrixinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##The output of the makeCacheMatrix.R is the input argument of cachesolve.R

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x=list(), ...) {
##Reading the inverse matrix of 'x'  
  matrixinverse <- x$getinverse()
##Checking if it has been computed already  
  if(!is.null(matrixinverse)) {
    message("getting cached inverse matrix")
    return(matrixinverse)
  }
##Reading matrix 'x'  
  data <- x$get()
##Computing the inverse matrix of 'x'  
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}
