## Put comments here that give an overall description of what your
## functions do

## this function creates a cache matrix object

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the inverse to NULL 
  inv <- NULL 
  ## define the set function that will be used 
  ## to assign a new matrix value in the parent env
  ## if this is new matrix, reset the inverse to NULL
  set <- function(y) {                  
    x <<- y                             
    inv <<- NULL                        
  }
  ## Returns value of the cached matrix argument
  get <- function() x                   
  ## assigns value of inverse in parent environment
  setinverse <- function(inverse) inv <<- inverse  
  ## gets the value of inv where called
  getinverse <- function() inv
  ## returns list of 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## This function attempts to invert a matrix by
## 1. check if the inverse has already been calculated
## 2. If so return it
## 3. Otherwise calculate the inverse and cache it
## Test by ...
## Create a matrix (example)
## -- set.seed(1234)
## -- r = r.norm(10000)
## -- test_matrix = (r, nrow = 100, ncol = 100)
## -- cacheSolve(makeCacheMatrix(test_matrix)) 
## 

cacheSolve <- function(x, ...) {
  # x is the output of the makeCacheMatrix()
  inv <- x$getinverse()
  ##if we have alread cached the matrix return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if we do not have a inverse cached
  ## get the matrix
  data <- x$get()
  ## invert it
  inv <- solve(data, ...)
  ## set the cache for the inverse
  x$setinverse(inv)
  ## output the inverse
  inv
}
