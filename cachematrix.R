## in combination, these functions calculate the inverse of a mmatrix and save the value in a variable that can be retrieved 
## this allows the value calculated to be retrieved multiple times without repeatedly performing the calculation

## This function creates a list of functions that is used by the cacheSolve function. 
## It also creates a variable m, that is set to null
## the functions "get" retrieves the value of x, the function "set inver" sets a value for m
## the function "getinver" retrieves the value of m, and the function "set" allows one to set the value of x. 
## these four functions are used by the function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinver <- function(inver) m <<- inver
  getinver <- function() m
  list(set = set, get = get,setinver=setinver, getinver=getinver)

  
}

## This function checks to see if m is NULL in order to establish if the matrix inverse has already been calculated
## if it hasn't been calculated, the function calculates the matrix inverse and stores the value in m
## if it has been calculated, m is displayed

cacheSolve <- function(x, ...) {
  m <- x$getinver()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matr<-x$get()
  m<-solve(matr)
  x$setinver(m)
  m
  
}
