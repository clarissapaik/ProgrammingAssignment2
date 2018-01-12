## Course: R Programming
## Project: Assignment 2
## By: Clarissa Paik

## This is a pair of functions that work together to store or clear the inverse of a given matrix, 
## and either a) retrieve or b) recalculate the value of the matrix inverse

## Initializes objects (i and x) and defines "getters and setter" functions as subsets of a list

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL 
    set <- function(y = matrix()) {  
      x <<- y 
      i <<- NULL 
    }

  get <- function() x
  setinverse <- function(solve) i <<- solve ##
  getinverse <- function() i
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve retrieves the value of i from the "getter" element, x$getinverse. If undefined (NULL),
## the function solves for i, the inverse of matrix x (defined here as x$get for scoping reasons).
## If i is defined, the function returns a message and prints its contents.

cacheSolve <- function(x, ...) {
    i <- x$getinverse    
    if(!is.null(i)) {
        message("Getting cached matrix")
        print(i)
    }
    data <- as.matrix(x$get())
    i <- solve(data, ...)
    x$setinverse(i)
    print(i)
}
