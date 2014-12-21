## Coursera R Programming - Programming Assignment 2

## makeChaceMatrix is a function that create a special creates a special vector
## that can store the values of a matrix and cache the values of the matrix inverse.
##
## It is a list of the following funxtions:
## 1.  set: set the values of the matrix
## 2.  get: get the values of the matrix
## 3.  setinverse: set the values of the inverse of the matrix
## 4.  getinverse: get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the Inverse Matrix to NULL
  inv <- NULL

  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## get the value of the inverse of the matrix
  getinverse <- function() inv

  ## return list
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve return the inverse of the matrix 'x'either taking it from the cache
## or calculating it if not cached and storing it to cache afterward

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'  
  
  inv <- x$getinverse()
  ## Check if the Inverse Matrix is already chached and ready to be returned
  if(!is.null(inv)) {
    message("Retrieving cached data...")
    return(inv)
  }
  data <- x$get()
  ## Calculate the Inverse Matrix using the standard function solve()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
