## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The below function, creates a matrix, which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the matrix inverse
# get the value of the matrix inverse

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

  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

# Try with square matrix
x <- rbind(c(1, 2, 3), c(0.5, 6, 9.9), c(0.5, 0.1, 4))
mat <- makeCacheMatrix(x)
cacheSolve(mat)

