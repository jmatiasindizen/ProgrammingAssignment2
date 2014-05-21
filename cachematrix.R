## This code allow calculate inverse matrix of a input bidimensional 
## using cached values if is possible


## makeCacheMatrix takes a bidimensional matrix and returns a vector
## containing set, get, setinverse and getinverse functions

makeCacheMatrix <- function(x = matrix()) {
  ## m init
  m <- NULL
  
  ## function to set x value and init m
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## function to get x value
  get <- function() x
  
  ## function to set inverse value
  setinverse <- function(inverse) m <<- inverse
  
  ## function to get inverse value
  getinverse <- function() m
  
  ## list with all defined functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve takes a vector like makeCacheMatrix funcitons returns
## and return inverse matrix of input argument, using cached value 
## if is possible


cacheSolve <- function(x, ...) {

  ## first get inverse matrix from x parameter
  m <- x$getinverse()
  
  ## if inverse have been calculated (m is not null) yet it is used 
  ## cached data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if inverse have not been calculated it is obtained
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
