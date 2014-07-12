## Provides a function to create a cache for matrixes
## that will cache the matrix's inverse,
## a function for retrieving cached inverses
## and calculating and setting the cached inverse
## if it has not been calculated,
## and lastly a function to test the two.

## Creates a cache for a matrix to store its inverse.
## Provides accessors and mutators for the matrix
## and its inverse
## x - the matrix to use

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  ## set - will set the matrix in the cache
  ##       and mark the cached inverse as dirty
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  
  ## get - will retrieve the cached matrix
  get <- function() x
  
  ## setInverse - will set the cached inverse
  ##              of the current matrix
  ##              (it does not check that this is the inverse)
  setInverse <- function(i) inverse <<- i
  
  ## getInverse - gets the cached inverse,
  ##              or NULL if the cache is dirty
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Gets the inverse of x
## calculating and updating it if it has a dirty cache

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("Getting cached inverse.")
    return(i)
  }
  
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
}

## Test function that will run the above functions
## n - the number of times to test data
## m - the number of rows and columns in the tested matrix
testCacheableMatrix <- function(n = 3, m = 5) {
  mat <- makeCacheMatrix()
  
  while(n > 0) {
    print("Setting matrix")
    mat$set(matrix(rnorm(m*m), ncol=m, nrow=m))
    print(mat$get)
    
    print("Getting inverse first time")
    print(cacheSolve(mat))
    
    print("Getting inverse second time")
    print(cacheSolve(mat))
    
    n <- n - 1
  }
}
