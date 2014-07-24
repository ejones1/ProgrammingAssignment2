## The following functions will cache the inverse of a given matrix by using the solve function.

## The makeCacheMatrix function creates a special "matrix" object that can cache its
## inverse by running a subfunction that sets the value of the matrix,
## gets the value of the matrix, sets the value of the inverse via solve, and gets the 
## value of the inverse via solve.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The cacheSolve function gets the inverse of the special "matrix" returned by the 
## makeCacheMatrix function. The function first checks to see if the inverse has 
## already been created (i.e. if solve function has been run on it).  If the inverse
## has already been calculated and the matrix has not changed, then cacheSolve
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s  ## Returns a matrix that is the inverse of 'x'
}
