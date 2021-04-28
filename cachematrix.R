## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## NULL objects are assigned to "inv" variable. 
## the matrix's value is  set on "set" function.
## y is assigned to x through "<<-" in order to modify the upper level's variables
## through lower level's variables.
## "get" function gets the matrix "x"
## "setInverse" function sets the reverse matrix's value
## "getInverse" function returns the reverse matrix from x

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get =get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Inverse matrix's vallue is assigned to "inv" variable
## The "if" & "mat" functions will return the inverse matrix's value if already 
## computed. Else it will compute its value through the "inv <- solve(mat, ...)"
## line. "x$setInverse(inv)" line will assign the calculated reverse matrix's 
## value and return it through "inv" on the next line.
## inverse matrix must be square, also it won't work if it's singular. 
cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
