## 
## cachematrix.R 
## 
## Two functions that are used to create a special object that 
## stores a matrix and cache's its inverse.
## It assumes that the matrix supplied is invertible. 
## 

## makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to set / get the value of the matrix and to
## get / set the value of the inverse. 
## 
## Test case: 
## Create an invertible matrix e.g. 
## amat <- matrix(c(1, 3, 33, 11, 1, 2, 3, 4, 5), 3, 3)
## cmat <- makeCacheMatrix(amat)
## cacheSolve(cmat)
## cacheSolve(cmat)
## amatinv <- solve(amat) 
## cmatinv <- cacheSolve(cmat)
## amatinv - cmatinv 
## 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) inv <<- solve
  get_inverse <- function() inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cachesolve calculates the inverse of the special "matrix" created with the
## makeCacheMatrix function defined above. It first checks whether the inverse
## has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise it calculates the inverse and sets the 
## value of the inverse in the cache via the set_inverse function. 

cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$set_inverse(inv)
    inv
}
 