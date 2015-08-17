## Function makeCacheMatrix is used to create special matrix which can be cached and
## the function cacheSolve is used to check if the matix is inversible and calculate the 
## inverse and send to the function makeCacheMatrix to cache it

## makeCacheMatrix is used to create special Matrix which will be used to calculate its
## inverse. It returns  the list of four functions which is used setting and getting the
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {

  invMatrix <- NULL  
  set <- function(y){ ## Used for setting the special matrix
    x <<- y
    invMatrix <<- NULL
  }
  get <- function(){ ## used for getting the special matrix
    x
  }
  setInverse <- function(inverse){  ## Used for setting the inverse of the special matrix
    invMatrix <<- inverse
  }
  getInverse <- function(){ ## Used for getting the inverse of the special matrix
    invMatrix
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) ## returns a list of getters and setters
}


## cacheSolve function takes the list of functions from the makeCacheMatrix object as
## argument and returns a matrix. If the matrix is already cached previuosly and the
## original matrix is not changed, it returns the Inverse matrix from the cache.
## Otherwise it checks the matrix created using makeCacheMatrix method and checks 
## if it is a square matrix and determinant (|A|) is not zero. If both the conditions 
## justifies, it returns the inverse of the matrix and also caches it
## in the makeCacheMatrix method for future calls. Else it returns a 1X1 matrix with the
## error message for the reason inverse not being created.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  printError <- NULL
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) ## Checks if the cache contains the inverse, then returns it
  {
    message("Getting cached Inversed Matrix")
    return(invMatrix)
  }
  originalMatrix <- x$get()
  if(nrow(originalMatrix) == ncol(originalMatrix)) ## check to see if a square matrix
  {
    if(det(originalMatrix) != 0) ## check to see if |A| is not zero
    {
      invMatrix <- solve(originalMatrix) ## compute the inverse of the matrix
      x$setInverse(invMatrix) ## Sets the inverse in caching
      return(invMatrix)
    }
    else
      printError <- "The Matrix whose determinant is 0 cannot be inversed"
  }
  else
    printError <- "Only square matrices can be inversed"
  
  errorMatrix <- matrix(printError,1,1) ## creates and returns the matrix with error line
  errorMatrix
}
