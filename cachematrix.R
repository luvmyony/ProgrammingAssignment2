## Assignment2: Calcuating Inverse Matrix 

## This function creates a special "matrix" object that can cache its inverse.

#library("MASS"): use in case you want to get the inverse matrix via ginv function 

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) I <- solve
  getinverse <- function() I
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  mat <- x$get()
  I <- solve(mat, ...)
  x$setinverse(I)
  I
}