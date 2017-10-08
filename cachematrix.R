##Example Usage
##create a square matrix
##a<-makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2));
##> summary(a)
##Length Class  Mode    
##setMatrix    1      -none- function
##getMatrix    1      -none- function
##cacheInverse 1      -none- function
##getInverse   1      -none- function
##> a$getMatrix()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(a)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##at the second time we've got the cached value
##getting cahced data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5


## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      #cached inverse of a matrix      
      cache <- NULL
      #store a matrix
      setMatrix <- function(newValue){
              x <<- newValue
              cache <<- NULL
      }
      #returns the stored matrix
      getMatrix <- function(){
              x
      }
      #cache the given argument 
      cacheInverse <- function(solve){
              cache <<- solve
      }
      #get the cached value
      getInverse <- function(){
              cache
      }
      #return a list of functions for a matrix
      list(setMatrix = setMatrix,
           getMatrix = getMatrix,
           cacheInverse = cacheInverse,
           getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.


cacheSolve <- function(y, ...) {
        #get the cached value
        inverse <- y$getInverse()
        #return cahced matrix inverse if it's been already computed
        if(!is.null(inverse)){
                message("getting cahced data")
                return(inverse)
        }
        #otherwise get the matrix, calculate its inverse and store it in the cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        inverse
}
