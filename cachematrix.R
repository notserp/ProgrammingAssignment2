## These two functions together will take a matrix, (assuming that it is possible to calculate the inverse of 
## said matrix) and calculate the inverse. If the same matrix is submitted multiple times then the function caches
## the inverse matrix and returns it the second time avoiding recalculation.

## This functrion creates a special matrix which is really a list containing functions to set/get 
## the matrix and inverse matrix value.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function calculates the inverse of the matrix, after checking if inverse has already been calculated.
## If it has been calculated before it returns the cached value.
cacheSolve <- function(x, ...) {
  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  return(m)
  
  ## Return a matrix that is the inverse of 'x'
}

