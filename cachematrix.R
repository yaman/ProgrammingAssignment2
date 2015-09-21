## makeCacheMatrix function creates a new cachable matrix which will
## store its inverse. Having all functions as list is a very good trick,
## so that you can access it via x$set() or x$get()

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y = matrix()){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  setinv <- function(inverseM) inv <<- inverseM
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv  = getinv)
}


## this part is interesting.. 
## cacheSolve function checks if the passed modified matrix has its inverse
## stored by calling x$getinv() function
## if the result is not null, yay! it is a cache hit...
## else, it calculates the inverse of the matrix and stores it for 
## future use in the matrix itself by x$setinv(inv) function.

cacheSolve <- function(x, ...) { 
  inv <- x$getinv()
  
  if(!is.null(inv)){
    message("getting cached inv matrix")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
