## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcache <- function(cacheValue) m <<- cacheValue
  getcache <- function() m
  list(
    set = set,
    get = get,
    setcache = setcache,
    getcache = getcache
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrixValue <- x$getcache()
  if (!is.null(invMatrixValue)) {
    message("getting cached data")
    return(invMatrixValue)
  }
  data <-x$get()
  invMatrixValue <- solve(data, ...)
  x$setcache(invMatrixValue)
  invMatrixValue
}