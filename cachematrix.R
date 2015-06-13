## caching the inverse of a matrix rather than compute it repeatedly

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     inverse_matrix <- NULL
     get <- function() x
     setmatrix <- function(mat) inverse_matrix <<- mat
     getmatrix <- function() inverse_matrix
     list(set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix and caches it

cacheSolve <- function(x, ...) {
     inverse_matrix <- x$getmatrix()
     
     ## Check if inverse exists. if exists return cahced result
     if(!is.null(inverse_matrix)) {
          message("getting cached data")
          return(inverse_matrix)
     }
     
     ## if inverse does not exist, compute the inverse and cache it
     matrix <- x$get()
     inverse_matrix <- solve(matrix)
     x$setmatrix(inverse_matrix)
     inverse_matrix
}
