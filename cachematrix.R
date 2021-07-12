## The functions below are used to store a matrix and its inverse in a single object.
## Its main goal is to verify if the matrix has a inverse calculated already, so you don't need to processe 
## the operation again.

## MakeCacheMatrix create an S3 object.
## Its constructor receives a matrix, and creates geters and setters for 
## the matrix and its inverse, storing in in a list.

makeCacheMatrix <- function(x = matrix()) {
  ## returns a special matrix object that also stores its inverse if solved.
  inverse_matrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inv) inverse_matrix <<- inv
  
  getinv <- function() inverse_matrix
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve gets a makeCacheMatrix and Tests if the object has the inverse matrix stored
## If the matrix is stored, it returns the matrix, if not, it calculates it, stores it in the cache and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  
  x$setinv(m)
  m
}
