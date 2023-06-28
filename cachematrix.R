## Put comments here that give an overall description of what your
## functions do

## first create a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #set the values of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x #get the value of the matrix
  setinverse <- function(solve) m <<- solve #set the inverse of the matrix
  getinverse <- function() m #get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## solve the inverse of the created matrix from makeCacheMatrix
## first check if there is something in cache already
## if yes, use the cached value
## in no, solve the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m      ## Return a matrix that is the inverse of 'x'
}
