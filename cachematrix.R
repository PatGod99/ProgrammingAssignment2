# makeCachMatrix and cacheSolve
# created by: Luis Daniel Sánchez Godínez


## MakeCacheMatrix function creates a special matrix and cache its inverse
# Creates a list (or a 4x1 Matrix)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve function computes the inverse if it has not already been calculated

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i

}

# Test

M <- matrix(c(12,1,21,55, 2, 21, 21, 21,21 ),3,3)
M
x <- makeCacheMatrix(M)
x
cacheSolve(x)

