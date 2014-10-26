## This is a simple adaptation of the example of caching the mean of a vector
## to the setting of caching the inverse of a matrix
##
## Comments follow Google's R Style Guide


makeCacheMatrix <- function(x = matrix()) {
  # Creates a special "matrix" object that can cache its inverse
  # 
  # Args:
  #   x: Matrix assumed to be invertible.
  #      Default is matrix() - the 1x1 matrix with a single NA entry
  #
  # Returns:
  #   A list containing functions
  #    set: set the value of the matrix
  #    get: get the value of the matrix
  #    setinverse: set the value of the inverse
  #    getinverse: get the value of the inverse  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  } 
  get <- function() {
    x
  }
  setinverse <- function(inv)  {
    i <<- inv
  }
  getinverse <- function() {
    i
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # Compute and cache the inverse of a special "matrix" object created with makeCacheMatrix()
  #
  # Args:
  #   x: a special "matrix" object created with makeCacheMatrix()
  #
  # Returns:
  #   The matrix inverse of x
  #   If inverse is cached, prints the message "getting cached data" and returns cached inverse
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Code for testing

m<-makeCacheMatrix(replicate(4,rnorm(4)))
cacheSolve(cm)
cacheSolve(cm)
solve(m$get())

