## The functions perform the following:
## makeCacheMatrix: This function creates a special "matrix" object
##                  that can cache its inverse.
## cacheSolve:      This function computes the inverse of the special "matrix" returned 
##                  by makeCacheMatrix above. If the inverse has already been calculated 
##                  (and the matrix has not changed), then the 
##                  cachesolve should retrieve the inverse from the cache.
## 

makeCacheMatrix <- function(x = matrix()) {
  # initialize the stored inverse value to NULL
  inv <- NULL
  
  # to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL   # since the matrix changed
  }
  # to get the value of the matrix
  get <- function() x
  # to set the inverse
  setinv <- function(inv_) inv <<- inv_
  # to get the inverse
  getinv <- function() inv
   
  # return a list of all the above functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
      ## Return a matrix that is the inverse of 'x'
      ptm <- proc.time()
      ## Get Matrix "m" from Cache
      m <- x$getinv()             
      ## Check if not null, Return value read from Cache
      if(!is.null(m)) {
        message("getting cached data")
        cat("Process Time", proc.time() - ptm)
        return(m)
      }
      ## if "m" is NULL, get original Matrix
      mydata <- x$get()
      ## Calculate inverse using Solve
      m <- solve(mydata)
      x$setinv(m)
      cat("Process Time", proc.time() - ptm)
      ## Return inverse
      m
      
  
}