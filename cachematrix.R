## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that:
## 1. set the matrix.
## 2. get the matrix.
## 3. set the inverse.
## 4. get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <- i
  getinverse <- function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cachesolve is a function that calculates the value of the
## inverse of the matrix inputed. If the inverse has been computed
## it will get the cached data

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setinverse(inverse)
  inverse
  ## Return a matrix that is the inverse of 'x'
}

