## This function, in its entirety, caches the inverse of a matrix. 


## The makeCacheMatrix function creates a matrix that is able to 
## cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list (set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## The cacheSolve function computes the inverse of the matrix 
## created above, or retrieves its inverse from the cache. 

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)){
    message ("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s       
}
