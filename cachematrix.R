## Building a matrix inversion could take a lot of time. To minimize 
## the computation time of building a matrix inversion
## you can use the 'makeCacheMatrix' function to store a matrix 
## in the cache. Later on you can check in the second function
## whether the matrix inversion was already built. Then the 
## function takes the matrix inversion that already exists in the 
## cache. So the function do not has to build it again.

## Creates a cache matrix that allows you to access on a matrix
## that exists in the cache

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y){
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) inverse_matrix <<- inverseMatrix
  getInverseMatrix <- function() inverse_matrix
  list(set = set, get = get, 
       setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
}


## If there already exists a matrix in the cache, the function
## returns the existing matrix inversion. Otherwise the function
## builds a new matrix inversion and returns its value.

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getInverseMatrix()
  if(!is.null(inverse_matrix)){
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setInverseMatrix(inverse_matrix)
  inverse_matrix
}
