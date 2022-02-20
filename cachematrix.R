## February 17, 2022
## Programming Assignment 2 - Cache, JHU Course 2 R Programming
## Caching the inverse of a matrix
## Functions to compute the inverse of a matrix with caching/Lexical scoping

## the first function
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y         ## search for x in parent environment
    inv <<- NULL    ## search for inv in parent environment
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function solves for the inverse of a matrix, assuming it is invertible.
## check first if it is not null (already calculated) retrieve a previously calculated inverse
## from cache
## otherwise, solves for the inverse using solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Get cache data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data) %*% data
  x$setinverse(inv)
}
