## A pair of functions that helps caching inverse of a matrix.
## The `makeCacheMatrix` function helps store a matrix along with its potentially
## cached inverse.
## The `cacheSolve` function either calculates the inverse for the very first time,
## or returns the cached version stored in the entity returned by makeCacheMatrix.


## Creates a special matrix object that also stores the inverse if
## already computed. It exposes four different methods to the outside world
## that allows users to set the matrix, get the matrix, set the inverse,
## and get the inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # Set the matrix to a new value, and also nullify
  # the cached inverse, since we don't know the inverse of this new matrix yet
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Return the actual matrix stored
  get <- function() x
  
  # Set the cached inverse of the matrix
  setinverse <- function(inv) inverse <<- inv
  
  # Get the cached inverse of the matrix
  getinverse <- function() inverse
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Goes through our representation of a matrix (created via `makeCacheMatrix`) and
## if the cached inverse is already present, returns it.
## Else, it computes the inverse and stores the cached value back, so that it's
## available to use the next time around.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached value")
    return(inv)
  }
  
  ## Compute the inverse, since the cached value was null
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  
  inv
}
