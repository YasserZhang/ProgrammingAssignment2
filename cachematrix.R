# The two functions below are used to calculate and cache the inverse of a matrix once and for all, a step very useful when dealing with the time-consuming
# procedure of calculating the inverse of a huge matrix.

# This function creates a special "matrix"
# object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # initialize a variable called inverse used to cache the inverse of the matrix.
  inverse  <- NULL
  # create a function to reset the matrix and reset the inverse variable
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  # retrieve the matrix
  get  <- function() x
  # cache the inverse of the matrix
  setInverse <- function(nvrs) inverse <<- nvrs
  # get the inverse of the matrix
  getInverse  <- function() inverse
  # return a list containing all functions created above
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # getting the cached inverse of the matrix no matter if it exists
  inverse  <- x$getInverse()
  # check if the inverse has already been cached
  if(!is.null(inverse)){
    message("getting cached data")
    return (inverse)
  }
  # if not, then calculate the inverse of the matrix and cache it.
  else {
    data <- x$get()
    inverse <- solve(data)
    x$setInverse(inverse)
    return (inverse)
  }
}
