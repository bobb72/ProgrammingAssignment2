# Some computations are time consuming, and if these computations are computed repeatedly
# (with an unchanged input) it may be better to cache the computation using the <<- 
# operator.
# Given an invertible matrix (square), the following functions with calculate the inverse
# of the matrix, or retrieve it from the cache.
# To run it:
# 1) Create a matrix eg "mat <- matrix(c(1,2,11,12), nrow = 2, ncol = 2)"
# 2) "CMatrix <- makeCacheMatrix(mat)"
# 3) "cacheSolve(CMatrix)"
# 4) Repeat step 3 to see it read from cache

#The first function, makeCacheMatrix creates a special "matrix", which is really a list
# containing a function to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse
# 4) get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve computes the inverse of the special "matrix" if it has not already been
# calculated (by "inv" getting the data stored in makeCacheMatrix). If it has already
# been calculated it will be retrieved from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}