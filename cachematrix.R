## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setI <- function(I) m <<- I
  getI <- function() m
  list(set = set, get = get,
       setI = setI,
       getI = getI)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		if(!is.null(x$getI())&&x$getI()==solve(x$get())) {
    message("getting cached data")
    return(x$getI())
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setI(m)
  m
}
