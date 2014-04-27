## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  # Initialize value to null
  matInv <- NULL

  set <- function(y) {
    x <<- y
    # Upon any changes to the matrix, set the new inverse equal to null pending an update.
    matInv <<- NULL
  }
  
  get <- function() x
  setInv <- function(inInverse) matInv <<- inInverse
  getInv <- function() matInv

  #Returns a list with the setter and getter methods. The value of the matrix itself is stored in the environment of these functions.
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # First, see what we have stored as the inverse
  matInv <- x$getInv()

  if(!is.null(matInv)) {
    message("getting cached inverse")
    return(matInv)
  }
  # The 'else' clause is implied, as the function will end after the return.
  #retrieve the matrix
  mat <- x$get()
  # The inverse is calculated with the 'solve' function.
  matInv <- solve(mat)
  # Save the result back into our CacheMatrix object
  x$setInv(matInv)

  # Return the inverse
  matInv
}
