## This code allow calculate inverse matrix of a input bidimensional 
## using cached values if is possible


## makeCacheMatrix takes a bidimensional matrix and returns a vector
## containing set, get, setinverse and getinverse functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve takes a vector like makeCacheMatrix funcitons returns
## and return inverse matrix of input argument, using cached value 
## if is possible


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
