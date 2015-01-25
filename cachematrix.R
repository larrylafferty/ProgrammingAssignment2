## R Programming Assignment #2
## This file defines 3 functions:  'testIt', makeCacheMatris' and 'cacheSolve
##
## 'testIt' is a test harness that executes 'makeCacheMatrix' and 'cacheSolve'
##
## 'makeCacheMatrix' provides a means for storing a matrix
##   'cachedM' is the source (origiinal) matrix
##   'flagInverse' is a boolean; when the cached matrix changes, 
##    'flagInverse' is set FALSE to indicate that the inverse should be
##    calculated
##
## 
makeCacheMatrix <- function(x = matrix()) {
  # Initialization
  cachedM <- x
  flagInverse <- FALSE
  # Set the value of the cached matrix
  setM <- function(x) {
    # Whenever the matrix value is set, we set the flag to FALSE
    # IF this flag is FALSE, the 'cacheSolve' function will invert the matrix
    flagInverse <<- FALSE
    cachedM <<- x
  }
  # Set the inverse value
  # Get value of the source (i.e., original anc cached) matrix
  getSourceM <- function() cachedM
  # Get value of the inverted matrix
  setFlag <- function(x) flagInverse <<- x
  getFlag <- function() flagInverse
  list(setM = setM,
       getSourceM = getSourceM, 
       setFlag = setFlag,
       getFlag = getFlag)
}

## 'cacheSolve' stores a matrix that is the inverse of 'x'
## The function argument should be the list of functions defined for
## the 'makeCacheMatrix' function
##
cacheSolve <- function(x, ...) {
  ## Initialization
  inverseM <- matrix()
  
  getInverse <- function() inverseM
  setInverse <- function(x) {
    # If the flag is false, recompute the inverse
    if(x$getFlag() == FALSE) {
      inverseM <<- solve(x$getSourceM())
      x$setFlag(TRUE)
    } 
    ## The inverse matrix is up-to-date
    else {
      inverseM
    }   
    
  }
  list(getInverse = getInverse,
       setInverse = setInverse)
}

# testIt is a test harness for makeCacheMatrix and cacheSolve

testIt <- function () {
  # Make a test matrix
  sMatrix <- matrix(c(1,2,3,4), nrow=2, ncol=2)
  # Test code
  print("TEST CASE 1:  Initialization")
  print("Original matrix")
  print(sMatrix)
  mcFunction <- makeCacheMatrix(sMatrix)
  print("Original matrix as cached")
  print(mcFunction$getSourceM())
  print("Value of flag")
  print(mcFunction$getFlag())
  csFunction <- cacheSolve(mcFunction)
  csFunction$setInverse(mcFunction)
  print("Inverted matrix")
  print(csFunction$getInverse())
  
  
  print("TEST CASE 2:  cacheSolve called when cache and inverse are in sync")
  cacheSolve(mcFunction)
  print("Matrix as cached")
  print(mcFunction$getSourceM())
  print("Set the inverse if needed")
  csFunction$setInverse(mcFunction)
  print("Inverted matrix")
  print(csFunction$getInverse())
  
  print("TEST CASE 3:  cacheSolve called when inverse must be recalculated")
  # Make a new test matrix
  newMatrix <- matrix(c(1,2,3,4,19,23,8,8,9), nrow=3, ncol=3)
  mcFunction$setM(newMatrix)
  print("Matrix as cached")
  print(mcFunction$getSourceM())
  print("Inverted matrix should be out of sync")
  print(csFunction$getInverse())
  # Set the inverse
  csFunction$setInverse(mcFunction)
  print("Matrices should be in sync")
  print(mcFunction$getSourceM())
  print(csFunction$getInverse())
}







