## caching the inverse of a matrix rather than compute it repeatedly

## creates a special "matrix" object that can cache its inverse
inverse_matrix <- NULL
makeCacheMatrix <- function(x = matrix()) {
##   Function to make Matrix
     makeMatrix <- function(x = matrix()) {
          
          set <- function(y) {
               x <<- y
               inverse_matrix <<- NULL
          }
          get <- function() x
          setmatrix <- function(mat) inverse_matrix <<- mat
          getmatrix <- function() inverse_matrix
          list(set = set, get = get,
               setmatrix = setmatrix,
               getmatrix = getmatrix)
     }
##   Function to cache the inverse matrix     
     cacheInvMatrix <- function(x, ...) {
          inverse_matrix <- x$getmatrix()
          print(inverse_matrix)
          if(!is.null(inverse_matrix)) {
               message("getting cached data")
               return(inverse_matrix)
          }
          matrix <- x$get()
          inverse_matrix <- cacheSolve(matrix)
          x$setmatrix(inverse_matrix)
          inverse_matrix
     }
     cacheInvMatrix(makeMatrix(x))
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     solve(x)
}
