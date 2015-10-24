## Put comments here that give an overall description of what your
## functions do

## This code will take a matrix will and cache its inverse

## Write a short comment describing this function

## makeCacheMatrix will take a matrix.  The function contains a 
## list of functions.
## makeCacheMatrix$set will store or change the value
## makeCacheMatrix$get will provide the stored value
## makeCacheMatrix$setcache is set to store matrix inverse
## makeCacheMatrix$getcache will provide the matrix inverse

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
## cacheSolve will take the matrix from makeCacheMatrix and check 
## if the inverse has been created.
## If it hasn't, it will calculate and store.

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