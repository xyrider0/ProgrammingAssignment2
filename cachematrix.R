## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates getter for original matrix
## Creates variable to store matrix inverse
## Creates getters and setters for storing matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(y) inv <<- y
  getInv <- function() inv
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function

## Returns inverse of matrix if previously calculated and stored within the matrix object class
## Otherwise calculates the inverse and stores the value as part of the original matrix object class
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m) %*% m
  x$setInv(inv)
  inv
}
