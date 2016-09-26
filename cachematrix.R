## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix sets up an object that can store and return the input matrix and its inverse
##
## cacheSolve checks the cacheMatrix for the inverse matrix value. If one hasn't been
## calclated, it calculates the matrix inverse and stores it in the cacheMatrix

##makeCacheMatrix
## given an input matrix, creates an object with the following:
##   object$get() - gets the stored input matrix
##   object$set() - stores (sets) the input matrix into the object
##   object$setInverse() - stores(sets) the inverse of the input matrix into the object
##   object$getInverse() - gets the stored inverse of the input matrix 
makeCacheMatrix <- function(x = matrix()) {
  xInv <<- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  setInverse <- function(xIn) {
    xInv <<- xIn
  }
  getInverse <- function() xInv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

##cacheSolve
## given an input CacheMatrix object created by the makeCacheMatrix
## this funtion checks to see if an inverse matrix has already been calculated and stores
## if one has been stored, it returns the inverse matrix
## if an inverse matrix has not been stored, the function caluclates the matrix inverse
## and stores it in the CacheMatrix object, then returns the inverse matrix 
cacheSolve <- function(x, ...) {
  xInv <-x$getInverse()
  if(!is.null(xInv)) {
    message("Getting the Inverse of the Matrix")
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data,...)
  x$setInverse(xInv)
  return(xInv)
}
