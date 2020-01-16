makeCacheMatrix <- function(x = matrix()) {
  matrix_i <- NULL
  set <- function(y) {
    x <<- y
    matrix_i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrix_i <<- inverse
  getinverse <- function() matrix_i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(matrix_i)) {
    message("getting cached data")
    return(matrix_i)
  }
  data <- x$get()
  matrix_i <- solve(data, ...)
  x$setinverse(matrix_i)
  matrix_i
}
