## this first function "makeCacheMatrix" creates a matrix nxn and caches its values for 
## inverse matrix. You could get the matrix, its inverse matrix, and set the inverse matrix
## So, I don't know why would you like to set a inverse matrix, but this function exists :)

##with the function "cacheSolve()" you can receive the inverse matrix of the parameter, but, before of that
##the function checks if there is already a inverse matrix in cache, if not, it calculates a new.

## this is the makeCacheMatrix that set a new matrix, get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() {x}
  setinverse <- function(solve) {s <<- solve}
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## this is the cacheSolve that check if a inverse exists, computes and set a new inverse 
##if necessary

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting inverse matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
