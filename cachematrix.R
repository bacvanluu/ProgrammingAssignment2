##################################################################
## The function makeCacheMatrix creates a list of four functions,
## with x as an argument.
## x is a symmetric invertible matrix defined by the user.
## 
## The four functions are: set, get, calcinverse, getinverse.
## set: sets the matrix
## get: gets matrix from cache
## setinverse: calculates inverse of matrix and writes it to cache
## getinverse: retrieves inverse of matrix from cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #clears the inverse i
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Defines the set function
  ## which sets the matrix x to be equal to y (defined by user)
  ## and clears the inverse i
  
  get <- function() x
  ## Defines the function get
  ## which retrieves the matrix x from cache
  
  setinverse <- function(inverse) i <<- inverse
  ## Defines the function setinverse
  ## which writes the calculated inverse i to cache
  
  getinverse <- function() i
  ## Defines the function getinverse
  ## which retrieves the previously calculated inverse i from cache
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ## collates all four functions in a list
  
}

###########################################################
## The function cacheSolve returns the inverse of a matrix.
## If the inverse exists in cache, it is retrieved and returned from cache;
## otherwise it is calculated, saved to cache and returned.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # This part checks whether the inverse has been calculated  
  # and returns the inverse if this is the case
  
  data <- x$get()
  i <- solve(data) %*% data
  ## Calculates the inverse of the matrix using the solve function
  ## solve(x) %*% x returns the inverse of matrix 'x'
  
  x$setinverse(i)
  i
  ## Writes the inverse of the matrix into cache
  ## and returns the matrix 
  
}

