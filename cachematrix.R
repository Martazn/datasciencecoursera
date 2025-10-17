#R programming 3: Lexical scoping
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # initialize the cached inverse as NULL
  
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # reset the cached inverse whenever the matrix changes
  }
  
  # get the matrix
  get <- function() x
  
  # set the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # get the cached inverse
  getInverse <- function() inv
  
  # return a list of all four functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # if inverse is already cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)  
  x$setInverse(inv)        # cache the inverse
  inv                      
}


#Example
mat <- matrix(c(2, 1, 1, 2), 2, 2)
cm <- makeCacheMatrix(mat)
cacheSolve(cm)
cacheSolve(cm)