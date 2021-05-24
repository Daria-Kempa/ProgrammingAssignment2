## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  temporary <- NULL
  set <- function(y){
    x <<- y
    temporary <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) temporary <<- inverse
  getInverse <- function() temporary 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this functio\n

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  temporary <- x$getInverse()
  if(!is.null(temporary)){
    message("getting cached data")
    return(temporary)
  }
  mat <- x$get()
  temporary <- solve(mat,...)
  x$setInverse(temporary)
  temporary
}