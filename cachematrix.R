## makeCacheMatrix function returns an list containing the following functions:
## 1) get: returns the matrix that was set on beforehand
## 2) set: replaces the existing matrix with a new one
## 3) getInverse: Returns the inverse matrix, but does not calculate it.
##    The actual calculations are made on cacheSolve function
## 4) setInverse: Setting the inversed matrix cache

makeCacheMatrix <- function(x = matrix()) {
  
  # Anitializing internal cache variable
  inverseMatrixCache <- NULL

  get <- function(){ x }
    
  set <- function(newMatrix){
    # Assigning new value to internal variable
    x <<- newMatrix
    
    # When my matrix is replaced/changed, we must clean the cache
    inverseMatrixCache <<- NULL
  }
  
  getInverse <- function(){ inverseMatrixCache }
  
  setInverse <- function(inverse){
    inverseMatrixCache <<- inverse
  }
  
  # Exposing functions to public calls
  list(get = get,
       set = set,
       getInverse = getInverse,
       setInverse = setInverse)
}


## cacheSolve function takes a makeCacheMatrix object as parameter and performs the following:
## 1) Checks if are there any value on makeCacheMatrix > inverseMatrixCache variable.
##    If there aren't any, calculate and store its value inside the makeCacheMatrix object.
## 2) Returns the inverse matrix.

cacheSolve <- function(x, ...) {
  
  # It will only calculate the inverted matrix if the cache doesn't exist
  if (is.null(x$getInverse())){
  
    # Printing a message informing that the cache is inexistent and that's being calculated.
    print("Cache MISS - Calculating inverse matrix...")
    
    # Assigning inverse matrix value to cached variable
    x$setInverse(solve(x$get()))
  }
  
  # Both if inverse matrix was calculated or grabbed from cache, its value will be returned
  x$getInverse()
}
