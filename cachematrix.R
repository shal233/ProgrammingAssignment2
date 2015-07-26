## R Programming assignment 2
## The following two functions make a pair to compute, store and retrieve a function on the 
## matrix. Here the function is the 'inverse of a matrix'. However, this matrix function pair 
## can be used only for a specific input matrix. Extra code is required for accommodating multiple 
## matrices' results.


## The first function, makeVector creates a special "matrix", which is containing a function to
## 1.set the value of the vector
## 2.get the value of the vector
## 3.set the value of the mean
## 4.get the value of the mean

makeCacheMatrix <- function(xm = matrix()) {
  #Initialize value of matrix mt
  mt <- NULL
  y <- NULL
  #Define function where the matrix will be cached
  setmatrix <- function(y) {
    xm <<- y
    #Change value of stored inverse if matrix was changed
    mt <<- NULL
  }
  #Retrieves matrix  
  getmatrix <- function() xm
  #Calculates inverse of matrix using 'solve'
  setinverse <- function(solve) mt <<- solve
  #Gets inverse
  getinverse <- function() mt
  #Passes value of function makeCacheMatrix
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above
## function. However, it first checks to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the value of the inverse in the cache via the setinverse 
## function.

## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  #Retrieves stored inverse result
  m <- x$getinverse()
  #If inverse result exists, its returned and function ends
  if(!is.null(m)) {
    message("getting cached matrix data")
    return(m)
  }
  #If no result exists, inverse is computed and stored
  data <- x$getmatrix()
  m <- solve(data)
  x$setinverse(m)
  m
}
