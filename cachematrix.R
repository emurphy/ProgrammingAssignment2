## use makeCacheMatrix to optimize large matrices where the inverse is
## needed more than once during processing. After wrapping your large matrix
## with makeCacheMatrix, call cacheSolve for the inverse. Repeat cacheSolve 
## as needed and marvel at the speed.
## 
## Sample usage:
## 
##   cacheable <- makeCacheMatrix(myLargeMatrix)
##   inverse   <- cacheSolve(cacheable)
## 
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Given a variable returned from makeCacheMatrix, cacheSolve returns the 
## inverse. Subsequent calls are cached.
## See ?solve for more details on calculating the inverse
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse  
}
