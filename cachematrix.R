## The following functions provide functionality to cache a matrix
## and also its inverse so as to improve read/load times when referencing
## the inverse


## makeCacheMatrix creates a special "vector" which is really a list containing
## several functions.  The four functions it contains are as follows:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix
##
## Note that functions 3 & 4 do not perform the inversion, this is done by the 
## subsequent function
makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve loads the inverse matrix variable to see if it is populated
## (i.e. this function has been previously called).  If so, the inverse matrix is 
## simply returned and the function ends.  Otherwise it inverses the matrix, stores it 
## into the inverse matrix variable and then returns it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting inversed matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}