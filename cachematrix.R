## The two functions makeCacheMatrix and cacheSolve basically store a matrix 
## and cache its inverse respectively


## what makeCacheMatrix does is that it set and get the value of the matrix,
## and then set and get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve basically stores or cache the inverse of the matrix from 
## makeCacheMatrix. when the inverse has already been calculated, it will 
## retrieve and return the value of the cached inverse with message "getting cached data".

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  ## Return a matrix that is the inverse of 'x'
  inverse
}
