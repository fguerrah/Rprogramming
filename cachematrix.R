## Put comments here that give an overall description of what your
## functions do

## Function that creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set_matrix <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get_matrix <- function() x
  set_inverse <- function(solve) inv_matrix <<- solve
  get_inverse <- function() inv_matrix
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## Function that computes the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, and the matrix has not changed, then
## the cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv_matrix <- x$get_inverse()
  if(!is.null(inv_matrix)) {
    print("retrieving cached matrix")
    return(inv_matrix)
  }
  data <- x$get_matrix()
  inv_matrix <- solve(data, ...)
  x$set_inverse(inv_matrix)
  inv_matrix
}
