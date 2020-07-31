## The functions given below cache the inverse of a given particular matrix. In other words, this function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix consists of set,get,setinverse and getinverse.

makeCacheMatrix <- function(x = matrix()) {
  # Returns a todo error if x is not a matrix
  inv <- NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL  
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}       

## Computes, returns and cache the inverse of a matrix returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
## Returns a matrix that is the inverse of 'x'
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

