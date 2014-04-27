## Functions to study the scoping rules and how to access
## the outer environment using <<- operator
## In practice we create a special 'matrix' list which
## holds certain functions to manipulate/monitor the matrix
## data which is stored in the function environment.  Then
## we create a second function to calculate the inverse
## of the matrix which checks whether it is already
## calculated -by calling the get function from the
## returned list - and if yes, retrieves and retruns the
## cached value.  If no, calculates, saves and retruns
## the inverse.

## makeCacheMatrix: this function returns a list of
## functions:
## 1)set the value of the vector
## 2)get the value of the vector
## 3)set the value of the mean
## 4)get the value of the mean
##


makeCacheMatrix <- function(x = matrix()) {
  ##creates the special matrix
  m <- NULL #holds the inverse of matrix x
  
  #this function sets the value of x, cleans up the inverse
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  #function to get the matrix
  get <- function() x
  #function to set the inverse value, m
  setInverse <- function(inverse) m <<- inverse
  #function to retrieve the inverse
  getInverse <- function() m
  
  #returned list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: this function returns the inverse of the 
## matrix (which is a special 'matrix' that was built with
## the first function).  This function checks whether the 
## inverse was already calculated by calling the built-in 
## function of matrix and if yes, it returns the stored 
## matrix, else it computes the inverse of the matrix and 
## returns it.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #checking whether the inverse is already calculated
  m <- x$getInverse()
  
  #if inverse is there in memory, return cached value
  if(!is.null(m)){
    message('getting cached data')
    return (m)
  }
  
  #if it is not in memory, get the matrix data, calculate
  #the inverse, set the value to m and return m to caller
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  
  m
}
