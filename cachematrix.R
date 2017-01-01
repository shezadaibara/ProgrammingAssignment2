## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cache_inverse <- NULL
  
  
  
  get <- function() x
  get_matrix_inverse <- function() cache_inverse
  
  set <- function(y) {
    x <<- y
    cache_inverse <<- NULL
  }
  set_matrix_inverse <- function(inverse) {
    cache_inverse <<- inverse
    return(cache_inverse)
  }
  
  list(set = set, get = get,
       set_matrix_inverse = set_matrix_inverse,
       get_matrix_inverse = get_matrix_inverse
       )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  result <- x$get_matrix_inverse()
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
  data <- x$get()
  result <- solve(data, ...)
  x$set_matrix_inverse(result)
  result
}
