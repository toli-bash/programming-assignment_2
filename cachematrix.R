## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse when the matrix is set
  }
  get <- function() x  # Return the matrix
  setInverse <- function(inverse) inv <<- inverse  # Set the cached inverse
  getInverse <- function() inv  # Get the cached inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {  # If the inverse is already cached, return it
    message("getting cached data")
    return(inv)
  }
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the inverse
}
EXPLANATION
makeCacheMatrix:

set: Sets the value of the matrix and resets the cached inverse.

get: Gets the value of the matrix.

setInverse: Sets the value of the inverse in the cache.

getInverse: Gets the cached inverse.

cacheSolve:

getInverse: Checks if the inverse is already cached and returns it if available.

get: Retrieves the matrix if the inverse is not cached.

solve: Computes the inverse of the matrix.

setInverse: Caches the computed inverse for future use.