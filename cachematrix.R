## These Function create a special kind of matrix which solves its inverse and save the results to cache


## The function provides a list of functions that set, get, set the solution or save the solution to cache

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


## This function accepts the above function as an argument and returnes the cached solution or calculates a solution if one does not exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  message("solving and saving to cache")
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
