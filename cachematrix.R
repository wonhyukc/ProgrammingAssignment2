##########
# coursera R
# Peer Assessments /Programming Assignment 2: Lexical Scoping 
# These pair of functions will cache the inverse of a matrix



## This function creates a special "matrix" object that can cache its inverse
## four sub-functions to make inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## This function computes the inverse of the special "matrix"
## returned by the above function. If the inverse has already
## been calculated (and the matrix has not changed), then the
## inverse will be retrieved from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m  
}




#########
# Let's test.
a<- makeCacheMatrix( matrix( 1:4, 2, 2) )
cacheSolve(a)
# return inverse matrix

cacheSolve(a)
# should return cached inverse matrix
