## With these functions we can compute and cache the inverse of a matrix and retrieve it if needed.

## makeCacheMatrix creates a "matrix object" that can cache its inverse.

makeCacheMatrix <- function(x = matrix(), ...) {
  inverse <- NULL
  setvalue <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getvalue <- function () x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function () inverse
  
  list(setvalue = setvalue, 
       getvalue = getvalue,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve retrieves the inverse from the cache, if it's stored there.
## If it's not stored in the cache, it computes the inverse of the special "matrix" returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$getvalue()
  inverse <- solve(data, ...) 
  x$setinverse(inverse)
  inverse
}
