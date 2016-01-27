## These function perform Matrix inversion followed by caching
## the inverse of the matrix in order to avoid repeated computation
## which can take up valuable processing power and time.  

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function returns the inverse of the matrix. 
## The function checks to see if the inverse has been
## previously calculated.  
## If it has then the result is obtained and the
## calculation is skipped.  
## If the inverse has not been calculated, then the function
## calculates the inverse and caches the result using the 
## setinverse function.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {message("getting cached data.")
    return(inv)
}
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
