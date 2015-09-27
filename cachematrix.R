## Mechanisim for caching the inverse of a matrix rather than compute it 
## repeatedly.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    inverseMatrix <<- inverse
  }
  
  getInverse <- function() {
    inverseMatrix 
  }
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inverseMatrix <- x$getInverse()
  
  ## Inverse of 'x' is already in cache and return
  if (!is.null(inverseMatrix)) {
    message("getting cached inverse")
    return(inverseMatrix)
  }
  
  ## Inverse is not found in the cache, so calculate and set it
  mainMatrix <- x$get()
  inverseMatrix <- solve(mainMatrix, ...)
  x$setInverse(inverseMatrix)

  ## Return newly caluculated inverse of 'x'
  inverseMatrix
}
