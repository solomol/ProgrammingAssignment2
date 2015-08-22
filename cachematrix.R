## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## start with "empty" cache
  inv <- NULL
  ## Set value of matrix, store in "x" in this environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## Return value of matrix stored in "x"
  get <- function() x
  ## Set Inverse of matrix, store into "inv"
  setinv <- function(inputinv) inv <<- inputinv
  ## Get Inverse of matrix from "inv"
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
## if mean hasnt already been set, set it, otherwise just get it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #else get matrix x using the object's get function
  data <- x$get()
  #then compute the inverse...
  inv <- solve(data, ...)
  #store result into object's cache
  x$setinv(inv)
  #return calculated result
  inv
}
