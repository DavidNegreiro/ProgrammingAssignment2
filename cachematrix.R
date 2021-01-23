## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { # insert x as a matrix
  # initialize the null
  inv <- NULL     
  # creates the matrix in the working environment
  set <- function(y) { 
    x <<- y
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() x  
  # invert the matrix and store in cache
  setInverse <- function() inv <<- solve(x) 
  # get the inverted matrix from cache
  getInverse <- function() inv
  # returns the inverse of the matrix to the working environment
  list(set = set,      
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  # return inverted matrix from cache if it exists
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # create matrix since it does not exist
  mat <- x$get()
  inv <- solve(mat, ...)
  # set inverted matrix in cache
  x$setInverse(inv)
  inv
}
