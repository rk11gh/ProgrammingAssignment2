## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" created by makeCacheMatrix.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    inv
  }
  
  mat <- x$get()
  
  inv <- solve(mat, ...)
  
  x$setInverse(inv)
  inv
}


matrixObject <- makeCacheMatrix(matrix(c(2, 1, 1, 2), 2, 2))

inverse <- cacheSolve(matrixObject)
inverse
cachedInverse <- cacheSolve(matrixObject)  # This will use the cached data
cachedInverse