## Title: Programming Assignment 2 - Lexical Scoping
## Week 2 Assignment (Peer Graded) of 4-Week R-Programming course;
## second course in the Data Science Specialization series

## Function: Creates a special "matrix" object that can cache its inverse
## Assumption: The matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    ## <<- operator which can be used to assign a value to an object
    ## in an environment that is different from the current environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Function: computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    ## Check if an inversed matrix has previously been computed
    print("An inversed matrix was previously computed!")
    return(inv)
  }
  ## Nope, calculate it for the first time
  data <- x$get()
  inv <- solve(data)
  ## For next time, preserve the inversed matrix in the cache
  x$setinverse(inv)
  inv
}


## 22 February 2015
