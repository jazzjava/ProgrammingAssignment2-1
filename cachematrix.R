## this R script file contains 2 functions to help cache matrixe inverse calculation


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i_m <- NULL
  ## set: set the matrix value
  set <- function(y) {
    x <<- y
    i_m <<- NULL
  }
  ## get: get the value of the matrix
  get <- function() x
  ## setinverse: set the value of matrix inverse
  setinverse <- function(solve) i_m <<- solve
  ## getinverse: get the value of matrix inverse
  getinverse <- function() i_m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i_m <- x$getinverse()
  ## We evaluate if the inverse is calculated and the matrix remains the same (absolute difference is zero).
  if(!is.null(i_m) && sum(abs(x -  x$get())) == 0) {
    message("getting cached data")
    return(i_m)
  }
  data <- x$get()
  i_m <- solve(data, ...)
  x$setinverse(i_m)
  i_m
}
