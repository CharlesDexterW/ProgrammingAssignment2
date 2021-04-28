## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## NULL objects are assigned to "wink" variable. 
## the matrix's value is  set on "set" function.
## y is assigned to x through "<<-" in order to modify the upper level's variables
## through lower level's variables.
## "get" function gets the matrix "x"
## "setInverse" function sets the reverse matrix's value
## "getInverse" function returns the reverse matrix from x

makeCacheMatrix <- function(x = matrix()){
  wink <- NULL
  set <- function(y){
      x <<- y
      wink <<- NULL
  }
  get <- function() {
    x
    }
  setInverse <- function(inverse) {
    wink <<- inverse
    }
  getInverse <- function() {
    wink
    }
  list(set = set, 
       get =get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Function Description
## Inverse matrix's vallue is assigned to "wink" variable
## The "if" & "mat" functions will return the inverse matrix's value if already 
## computed. Else it will compute its value through the "wink <- solve(dat, ...)"
## line. "x$setInverse(inv)" line will assign the calculated reverse matrix's 
## value and return it through "wink" on the next line.
 
cacheSolve <- function(x, ...){
  wink <- x$getInverse()
  if(!is.null(wink)){
    message("getting cached data")
    return(wink)
  }
  dat <- x$get()
  wink <- solve(dat, ...)
  x$setInverse(wink)
  wink
}

##=============================================================================
## inverse matrix must be square, also it won't work if it's singular.
## One Example: 
## example <- makeCacheMatrix(matrix(1:4, nrow = 2, ncol = 2))
## example$get()
## cacheSolve(example)
