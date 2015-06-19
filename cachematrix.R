## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly. 
## Underneath are a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix creates and returns a list of nested functions 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  compinverse <- function(inverse) cache <<- inverse
  getinverse <- function() cache
  list(set = set, get = get,
       compinverse = compinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  cache <- x$getinverse()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  matrix <- x$get()
  cache <- solve(matrix, ...)    
  x$compinverse(cache)
  ## Return a matrix that is the inverse of 'x'	
  cache
}