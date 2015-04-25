## Function accepts a matrix and caches it
## Has functions to set and get the cached matrix
## Also has setter and getter to return the cached
## inverse of the passed matrix

## Caches the given matrix and initialize the inverse
## (set scope of passed matrix and its inverse)
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse        
  inv <- NULL
  
  # set function caches the matrix 
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
  
  # get function returns the cached matrix
  get <- function () {
    x
  }
  
  # set inverse function 
  #setinverse <- function (invmat) {
  #  inv <<- invmat
  #}
  
  # set inverse and return the inverse matrix
  setinverse <- function (invmat) {
    inv <<- invmat
    
    # return the inverse
    inv
  }
  
  # get the cached the inverse
  getinverse <- function () {
    inv
  }
  
  # named list with pointer to functions 
  list (set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Compute the inverse of the passed matrix
## and set the inverse if not set
## Return the computed inverse
cacheSolve <- function(x, ...) {
  # get the inverse
  inv <- x$getinverse()

  # check if inverse is already computed
  # return inverse if already set
  if (!is.null(inv)) {
    message("getting the cached inverse")
    return (inv)
  }
  
  # hold the inverse in a temp variable
  #inverse <- solve(x$get(), ...)
  
  # set the inverse
  #x$setinverse(inverse)
  
  # return the inverse
  #inverse
  
  # compute and set the inverse
  x$setinverse(solve(x$get(), ...))
}
