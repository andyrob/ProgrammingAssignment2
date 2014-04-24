## functions to cache matrix inverse along with matrix data


## function makeCacheMatrix accepts matrix x as input and outputs a list of 
## 1) function to set the matrix data to x and the inverse to null 
## 2) function to return the matrix data
## 3) function setsolve to calculate the inverse of the matrix
## 4) function getsolve to return the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## function cachesolve checks "special matrix x" to see if the inverse is already calculated. 
## if it is, return the calc'd value. if not, calculate the inverse, store it in x and return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
