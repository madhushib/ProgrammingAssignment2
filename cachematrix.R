## This is to cache the computation of metrix inversion.
## Once an inverse of a matrix is calculated, it is stored and used repetitivelly.

## This initializes an object x which has a matrix and its inverse. 
##Once the matrix inverse is calculated and stored it can be retrieved multiple times.
##Avoid the repetitive calculations.


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


## This method returns inverse of MAtrix x
## The inverse is calculated only if it is not calculated previously and cached in the object x.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    print("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
