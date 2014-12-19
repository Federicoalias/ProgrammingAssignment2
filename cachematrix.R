## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

## Write a short comment describing this function
#  m <- NULL opens a place holder for the future value of the inv matrix

#set <- function(y)
#  {
#    x <<- y
#   m <<- NULL
#  }
#  defines a function to set the matrix, x, to a new matrix, y, and resets the inverse of the matrix, m, to NULL

#   get <- function() x returns the matrix x

#setinverse <- function(inverse) m <<- inverse 
#sets the inverse, m, to inverse

# getinverse <- function() m
# returns the inverse, m
# returns a list of the functions defined. 


makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Write a short comment describing this function
##The cacheSolve function calculates the inverse of the matrix created with the previous function. 
##However it first verifies if the inverse of the matrix has been already calculated. 
##If this is the case it gets the inverse from the cache and does not re-do the computation. 
##Otherwise, it calculates the inverse of the data and sets the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        
  if(!is.null(m)) {
  
    message("getting cached data")
    
    return(m)
  }
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinverse(m)
  m
}

