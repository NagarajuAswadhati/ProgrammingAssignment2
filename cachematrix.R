## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

The below function, creates a matrix, which is really a list containing a function to
set the value of the matrix
get the value of the matrix
set the value of the matrix inverse
get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse # Assing the value of inverse in cache
  
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## The following function calculates the inverse of the "matrix" 
## created with the above function. However, it first checks to see 
## if the inverse has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse 
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  inv
}