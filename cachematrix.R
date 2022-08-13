## Put comments here that give an overall description of what your
## functions do
## The function 'makeCacheMatrix' and 'cacheSolve' make up a pair of functions which can cache the inverse of a matrix

## Write a short comment describing this function
## This function can creates a matrix object which can cache its inverse matrix.
makeCacheMatrix <- function( x = matrix() ) {
  
  ## initialize
  m <- NULL
  
  ## set the matrix
  set <- function( matrix ) {
    x <<- matrix
    m <<- NULL
  }
  
  ## get the matrix
  get <- function() {x}
  
  ## set the inverse matrix
  setinverse <- function(inverse) {
    m <<- inverse
  }
  
  ## get the inverse matrix
  getinverse <- function() {
    m
  }
  
  ## return result
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function can compute the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## return the inverse matrix of x
  m <- x$getinverse()
  
  ## if m is already been calculated, return the inverse from the cache and skips the computation. 
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## get the matrix from the object
  data <- x$get()
  
  ## compute the inverse using solve function
  m <- solve(data)
  
  ## set the inverse to the object
  x$setinverse(m)
  
  ## return result
  m
}
