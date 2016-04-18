## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {      m <- NULL  #sets the value of m as Null
    set <- function (y){ #sets the value of the matrix
      x <<- y
      m <<- NULL
    }
    get <- function () x # gets the value of the matrix
    setmatrix <-function(solve) m <<- solve #sets the value of the inverse
    getmatrix <- function() m  #gets the value of the specified matrix
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
  }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        cacheSolve <- function(x, ...){
  m <- x$getmatrix () #retrieves the value of matrix if it exists before
  if(!is.null(m)){
      return (m) #if the matrix does not exist, return the specified value of m
  }
  data <- x$get() #set given matrix as data
    m <- solve(data, ...) #compute inverse of given matrix (data)
    x$setmatrix(m) #set the iverse matrix to cache
    m #return value of m or matrix
}

