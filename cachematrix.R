makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  set.inverse <- function(inverse) m <<- inverse
  get.inverse <- function() m
  list(set = set, get = get, set.inverse = set.inverse, get.inverse = get.inverse)
}

cacheSolve <- function(x, ...) {
  m <- x$get.inverse()
  if (!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$set.inverse(m)
  m
}
