## To avoid recomputation, these two functions serve as initially caching the inverse 
## of a matrix and then returning that value from cache if it's already computed

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv.mat <- function(solve) m <<- solve
  getinv.mat <- function() m
  list(set = set, get = get, setinv.mat = setinv.mat, getinv.mat = getinv.mat)
}


## This functions retrieve the inverse of a matrix from cache

cacheSolve <- function(x, ...) {
  m <- x$getinv.mat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv.mat(m)
  m
}