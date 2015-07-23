##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ##get the value of the matrix
  get <- function() x
  
  ##set the value of the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ##get the value of the inverse of the matrix
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachSolve calculates the inverse of the special "matrix" created above
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  ##checks to see if the inverse has already been calculated
  if(!is.null(inv)) {
    
    ##if it has been calculated, retrieves from the cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  ##If no cached data, calculates the inverse
  inv <- solve(data, ...)
  
  ##and sets the value of the inverse in the cache
  x$setinverse(inv)
  inv
}