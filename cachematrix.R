## This file has two functions that help cache the inverse of a matrix.
## Caching avoids repeating the same calculation many times.

## This function creates a special "matrix" object that can store its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This will store the inverse later
  
  # Set a new matrix and clear the old inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Get the matrix
  get <- function() x
  
  # Set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getinverse <- function() inv
  
  # Return a list of the four functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function returns the inverse of the matrix from makeCacheMatrix.
## If the inverse is already stored, it gets it from the cache.
## If not, it calculates the inverse and stores it.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # If inverse is already stored, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Else, get the matrix, solve it, and store the result
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
