## 
## cachematrix.R 
## 
## Two functions that are used to 1) create a special object that stores
## a matrix and cache's its inverse and 2) get the cached inverse if present
## or calculate the inverse and store it, then return the inverse.
## It assumes that the matrix supplied is invertible. 
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


## makeCacheMatrix creates a special "matrix", which is really a list 
## containing functions to set and get the value of the matrix and to
## set and get the value of the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInv <- function(solvedInv) inv <<- solvedInv
    
    getInv <- function() inv
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cachesolve calculates the inverse of the special "matrix" created with the
## makeCacheMatrix function defined above. It first checks whether the inverse
## has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise it calculates the inverse and sets its  
## value in the cache via the setinv function. 

cacheSolve <- function(x, ...) {
    
    # Try to get the calculated inverse from the cache.
    # If successful then return the cached inverse. 
    
    inv <- x$getInv()
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise get the matrix data, calculate the inverse,
    # cache the inverse, and return it. 
    
    data <- x$get()
    
    inv <- solve(data, ...)
    
    x$setInv(inv)
    
    inv
}
 