## The following code describes two functions


## creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## sets default if values not already set
  setmatrix <- function(y) {
    x <<- y ## caches the inputted matrix so that cacheSolve can check whether it has changed
    m <<- NULL # # sets the value of m (the matrix inverse if used cacheSolve) to NULL
  }
  getmatrix <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix, # creates a list for the four functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the return by makeCacheMatrix.
## If already calculated, retrieves inverse from cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() # pulls inverse if already calculated
  if(!is.null(m)){ # check to see if cacheSolve has been solved before
    message("getting cached data")
    return(m)
    }
    
    matrix <- x$getmatrix() # run the getmatrix function to get the value of the input matrix
    m <- solve(matrix, ...) # compute the inverse of the input matrix
    x$setinverse(m) # caches the inverse using setinverse function
    m # return the inverse
    
}
