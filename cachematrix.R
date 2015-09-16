## These functions create and manage a special matrix capable of caching its inverse
## as long as the matrix doesn't change, there's no need to recalculate the inverse,
## so the "special matrix" returns the cached inverse instead of recalculating

## Example:
## > x <- makeCacheMatrix(matrix(c(3, 2, 2, 3),2,2))
##
## > x$get()
##      [,1] [,2]
## [1,]    3    2
## [2,]    2    3
##
## > cacheSolve(x)
##      [,1] [,2]
## [1,]  0.6 -0.4
## [2,] -0.4  0.6
##
## > x$getInverse()
##     [,1] [,2]
## [1,]  0.6 -0.4
## [2,] -0.4  0.6

## This function creates and returns a special matrix capable of caching its inverse.
## The argument must be an inverseable matrix, if omitted it will be an empty matrix
makeCacheMatrix <- function(x = matrix()) {
  ## This stores the cached inverse of the matrix
  inverse <- NULL
  
  ## Replaces the matrix (and sets the cached inverse to NULL)
  set <- function (y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## Returns the matrix
  get <- function () x
  
  ## Sets the (new) cached inverse
  setInverse <- function (s) inverse <<- s
  
  ## Returns the cached inverse
  getInverse <- function () inverse
  
  ## The list returned by makeCacheMatrix contains the functions to manage the matrix
  list (set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

  ## get the current cached inverse
  inv <- x$getInverse ()
  
  ## If there's no cached inverse, 
  if (is.null(inv)) {
    message ("calculating inverse")
    
    ## calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    ## Set it as current cached inverse
    x$setInverse(inv)
  }

  ## Return either the cached inverse if present,
  ## or the calculated one as above.
  inv
}
