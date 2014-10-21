## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## 'x' is a Invertible matrix
  
  ## Example Usage: 
  ## c = rbind(c(1, -1/4), c(-1/4, 1))  
  ## cash <-makeCacheMatrix(c)
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## Example Usage: 
  ## c = rbind(c(1, -1/4), c(-1/4, 1))  
  ## cash <-makeCacheMatrix(c)
  ## inv_c <- cacheSolve(cash)
  ## inv_c %*% c
  
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
