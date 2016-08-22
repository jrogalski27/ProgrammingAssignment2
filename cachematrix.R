## My function takes a matrix and calculates the inverse of it.  In order to reduce computation error, the value will be cached
## and the second function (cacheSolve) will evaluate if there is a cached result already.  
## If there is it will use the value, otherwise calculate the inverse from the start.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvert <- function(solve) m <<- solve
  getinvert <- function() m
  list(set = set,
       get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


cacheSolve <- function(x, ...) {
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvert(m)
  m
}
