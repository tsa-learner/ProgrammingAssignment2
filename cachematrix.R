## To cache the inverse of a matrix

## This function creates a special "matrix" object that can 
##cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invse <- NULL
  set <- function(y) {
    x <<- y
    invse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invse <<- inverse
  getInverse <- function() invse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invse <- x$getInverse()
  if(!is.null(invse)) {
    message("getting cached data")
    return(invse)
  }
  data <- x$get()
  invse <- solve(data, ...)
  x$setInverse(invse)
  invse
}
