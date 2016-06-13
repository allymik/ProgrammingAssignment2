## This function sets the value of an inverse matrix to a different global environment

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##set makes y into x, so set()
##get just returns x
##setinv invokes the function solve and the inverse it spits out is cached


##  This function checks if there is a value for an inverse matrix stored in cache and if there isn't, solves for an inverse matrix otherwise 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
