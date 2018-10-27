## TM 20181027: Assignment Month 2, Week 3
## These functions create a matrix and cache its inverse so that it doesn't need to be calc'ed more than once


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matrix1 = matrix()) 
{
  inverseOfMatrix <- NULL
  set <- function(y) {
    matrix1 <<- y
    inverseOfMatrix <<- NULL
  }
  get <- function() matrix1
  setInverse <- function(inverse) inverseOfMatrix <<- inverse
  getInverse <- function() inverseOfMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(matrixToInvert, ...) 
{
  if(is.null(matrixToInvert$getInverse()))
  {matrixToInvert$setInverse(solve(matrixToInvert$get()))}
  else matrixToInvert$getInverse()
  
}

## Test with the below code:
## setwd("C:/Users/t15bxn8/Desktop/DataSciences/Projects/Month2Week3/ProjectFromGit")
## example of an invertible matrix: matrix(c(1, 2, 3, 4), nrow=2, ncol=2) or matrix(c(1, 2, 3, 4, 7, 8, 9, 11, 19, 18, 17, 16, 57, 100, 200, 53), nrow=4, ncol=4)
## m <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow=2, ncol=2))
## m$get()  ## check that your matrix has been ingested properly into the internal variable
## m$getInverse() ## check that the inverse is currently null
## cacheSolve(m) ## calculate the inverse and cache it inside of m in the inverseOfMatrix var
## cacheSolve(m) ## run it again and now it returns what's inside the cache instead of calculating it
## m$getInverse()  ## check that your inverse has been cached by accessing m directly
## m$set(matrix(c(1, 2, 3, 4, 7, 8, 9, 11, 19, 18, 17, 16, 57, 100, 200, 53), nrow=4, ncol=4)) ## try with a bigger matrix
## m$getInverse() ## view that the old cached inverse has now been wiped out in m
## cacheSolve(m) ## calculate the inverse and cache it inside of m in the inverseOfMatrix var
## cacheSolve(m) ## run it again and now it returns what's inside the cache instead of calculating it
## m$getInverse()  check the results once again
