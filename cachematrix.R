## Functions necessary to cache a matrix and return the matrix or its inverse


## store matrix x in memory.  Includes typical getter and setter functions
## also includes a getinverse method to return the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(Inverse) {
      inverse <<- Inverse
  }
  getinverse <- function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


# return the inverse of a matrix if is in memory 
# OR
# gets data, computes the inverse, store it, show the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i  <- x$getinverse()
  
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  data  <- x$get()
  
  i  <- solve(data, ...)
  
  x$setinverse(i)
  
  i
}
