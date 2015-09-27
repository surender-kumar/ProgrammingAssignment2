## The makeCacheMatrix function makes a list with methods that set and get a matrix and its inverse in environment variable
## The cacheSolve function takes as arguments the list from the makeCacheMatrix function to calculate and set its inverse.  If the inverse is already set, then cached value is used

## makeCacheMatrix will create a matrix x, and creates setter/getter methods of x and inverse for x.

makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL ## initialize inverse
  
  set <- function(userValue = matrix()) {
    x <<- userValue 
    cachedInv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(invVal) {
    cachedInv <<- invVal 
    return(cachedInv)
  }
  
  getInverse  <- function() cachedInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## given the list from the makeCacheMatrix function,cacheSolve method will first check to see if there's already a cached inverse and return.Otherwise it attempt to solve its inverse and set/return it

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2),...) {
        ## Return a matrix that is the inverse of 'x'
		calculatedInverse <- x$getInverse() 
		if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
			message("We found cached data and saved valuable cpus!!!")
			return(calculatedInverse)
		}
		
		matrixToSolve <- x$get()
		calculatedInverse <- tryCatch({ 
    solve(matrixToSolve)
  }, warning=function(w) {
    message("This may not be the result you're looking for")
    message(w)
  }, error=function(e) {
    message("Something went wrong solving your matrix")
    message(e)
    message("\n")
  })
   message("Setting the value of inverse to:") 
  x$setInverse(calculatedInverse)
		
}
