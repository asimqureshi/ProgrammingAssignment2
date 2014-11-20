## @author Asim Qureshi

## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. These functions provide this support.

makeCacheMatrix <- function(x = matrix()) {
  # Creates a special "matrix" object that can cache its inverse.
  #
  # Args:
  #   x: Matrix whose inverse it to be cached
  #
  # Returns:
  #   A list of functions that will be later used by the cacheSolve(...) 
  #   function to cache the inverse
  
  m <- NULL
  
  # Setting the matrix data in the object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Getting the data from the object
  get <- function() x
  
  # Setting the inverse in the object
  setinv <- function(inv) m <<- inv
  
  # Getting the inverse from the object
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



cacheSolve <- function(x, ...) {
  # Computes the inverse of the special "matrix" returned by makeCacheMatrix 
  # above. If the inverse has already been calculated (and the matrix has not 
  # changed), then cacheSolve should retrieve the inverse from the cache.
  # 
  # Args:
  #   x: An object created using makeCacheMatrix()
  #   ...: Extra parameters to pass as along to the solve() function
  #
  # Returns:
  #   A matrix that is the inverse of 'x', possibly from the cache  
  
  m <- x$getinv()
  
  if( !is.null(m) ) { # if m is not null, so get inverse from cache and return
    
    message("getting cached data")
    return(m)
    
  }
  
  data <- x$get() ##otherwise compute the inverse, and put it in the cache
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
