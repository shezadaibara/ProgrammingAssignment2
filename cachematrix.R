## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cache_inverse <- NULL
  
  ## getter functions for matrix and its inverse
  get <- function() x
  get_matrix_inverse <- function() cache_inverse
  
  ## setter functions for matrix and its inverse
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
  ## check if matrix inverse is found in cache
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
  
  #get matrix and get its invers
  data <- x$get()
  result <- solve(data, ...)
  
  #save the inverse in the case
  x$set_matrix_inverse(result)
  
  result
}
