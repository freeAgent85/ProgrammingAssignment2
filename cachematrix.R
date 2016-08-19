## The following two functions work together to create and cache the inverse of
## a matrix.  First, the original matrix that we want to find the inverse of is
## fed into makeCacheMatrix().  Then, cacheSolve() can be called using the
## result of makeCacheMatrix to either return the cached inverse if it exists,
## or calculate and return the inverse of the original matrix.

## makeCacheMatrix() is a function that creates a list of subfunctions as well
## as the variable 'cachedMatrix' and x (the original parameter).

makeCacheMatrix <- function(x = matrix()) {
  cachedMatrix <- NULL
  set <- function(y) {
    x <<- y
    cachedMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) cachedMatrix <<- solve
  getinverse <- function() cachedMatrix
  ## The following list is returned, with all subfunction names kept as-is for
  ## use by cacheSolve
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve() checks to see if the inverse of a matrix that was fed into
## makeCacheMatrix() has already been calculated and stored in 'cachedMatrix'
## or whether it needs to be calculated.  If it needs to be calculated, the
## result will be stored in 'cachedMatrix' for future reference.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachedMatrix <- x$getinverse()
  if(!is.null(cachedMatrix)) {
    message("Getting cached matrix inverse...")
    return(cachedMatrix)
  }
  ## The following portion of the function only executes when there is nothing
  ## in cachedMatrix.
  data <- x$get()
  cachedMatrix <- solve(data, ...)
  x$setinverse(cachedMatrix)
  cachedMatrix
}
