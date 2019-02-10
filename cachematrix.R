## Assignment 2

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # store 2 things: x is the original matrix, i is the inverse
  i <- NULL
  
  # the 4 functions: set, get, setinverse, getinverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
        
  # return a list of the 4 functions
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() # try to get the inverse. hopefully, it was already calculated
  if(!is.null(i)) { # luckily, it was already calculated...
    message("getting cached data")
    return(i) # return it and go home
  }
        # if you're executing this, it means that the inverse was not cached, so calculate and store it:
  data <- x$get() # step 1: get the matrix, ...
  i <- solve(data, ...) # step 2: calculate the inverse
  x$setinverse(i) # step 3: store it for later usage
  i # stap 4: finally, it's over, return the blody inverse and go home
}
