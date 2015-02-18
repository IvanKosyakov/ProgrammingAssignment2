## Following pair of functions will cache the inverse of a matrix.

## Function makeCacheMatrix creates a special "matrix" 
##  object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  ## Function to set the value of the matrix.
  set <- function(y) { 
    x <<- y
    s <<- NULL
  }
  
  ## Function to get the value of the matrix.
  get <- function() x 
  
  ## Function to set the value of the inverse matrix.
  setSolve <- function(solve) s <<- solve
  
  ## Function to get the value of the inverse matrix.
  getSolve <- function() s
  
  ## Return list of functions.
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Function cacheSolve computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
##  inverse has already been calculated (and the matrix 
##  has not changed), then the cachesolve retrieves 
##  the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...) ## Let's assume that the matrix 
  ##  supplied is always invertible.
  x$setSolve(s)
  s
}
