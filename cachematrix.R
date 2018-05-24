## makeCacheMatrix and cacheSolve are functions for creating, retrieving and updatinga
## a cached inverse matrix.
## This can increase performance when the inverse matrix is used multiple times, as the cached value can be retrieved much faster
## than calculating the inverse matrix

## makeCacheMatrix
## input: a matrix
## returns an object with a list of 4 functions
## set(m) -> sets the value of a matrix to m. Upon each update, the cached inverse matrix is set to NULL
## get() -> retrieves a cached matrix
## setinv(solve) -> sets the inverse matrix value to "solve"
## getinv() -> returns the inverse matrix if set, otherwise NULL

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invm <<- solve
  getinv <- function() invm
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve
## input: x - a list created by makeCacheMatrix
## returns the inverse of the matrix in x
## if the value is cached, use the cache
## otherwise calculate the inverse, update the cache and return the inverse

cacheSolve <- function(x, ...) {

  invm <- x$getinv()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinv(invm)
  invm
  
        ## Return a matrix that is the inverse of 'x'
}
