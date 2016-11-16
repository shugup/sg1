## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. Below are two functions that are used to cache the 
## inverse of a matrix.

## The first function, makeCacheMatrix
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
          inverse <- NULL
         set <- function(y) {
                x <<- y
                 inverse <<- NULL
        }
         get <- function() x
         setinverseMatrix <- function(inverseMatrix) inverse <<- inverseMatrix 
         getinverseMatrix <- function() inverse
         list(set = set, get = get,
              setinverseMatrix = setinverseMatrix,
              getinverseMatrix = getinverseMatrix)
 }

## This function return the inverse of the matrix which we compute from first
## function.

cacheSolve <- function(x, ...) {
	  inverse <- x$getinverseMatrix()
         if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
         }
         data <- x$get()
         inverse <- solve(data, ...)
         x$setinverseMatrix(inverse)
         inverse
 }

## I have given example of matrix, which I have tested in above functions.

## x <- matrix(1:4,2,2)
## m = makeCacheMatrix(x)
## m$get()
##     [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## cacheSolve(m)
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

