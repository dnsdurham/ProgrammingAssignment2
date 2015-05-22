## There are two functions defined in this package:
## 1) makeCacheMatrix supports creating a special invertible
## matrix that is esposes a list of functions to get/set the matrix values
## and to get/set the inverse of the matrix
## 2) cacheSolve uses makeCacheMatrix to create the inverse of the makeCacheMatrix
## and store it in makeCacheMatrix before returning the value or return the cache of
## the inverted matrix if it exists in makeCacheMatrix

## makeCacheMatrix creates stores an invertible matrix and 
## enables caching of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Returns the inverse of matrix "x" either by solving the inverse
## (and storing in "x") or returning the cached inverted matrix
## in "x" if it exists
cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if(!is.null(im)){
    message("getting cached data")
    return(im)
  }
  matrix <- x$get()
  im <- solve(matrix, ...)
  x$setinverse(im)
  im
}
