## This function create a special matrix object that can cache its inverse
## The function cached a matrix and its inversed. This function will be usefull
## with de function cacheSolve. 

## This function create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}#End function makeCacheMatrix


## This function calculate the inverse of a squared matrix cached by makeCacheMAtrix
## function. If the inverse has already calculated, then the function retrive
## the inverse, instead of doing the calculation.
# The input for this function is the output of the function makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}#End cacheSolve
