## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The function makeCacheMatrix() created a special "matrix" object and enables
## caching of its inverse. It builds a list of functions to perform this.

makeCacheMatrix <- function(x = matrix()) {
   
     m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinv <- function(inv) m <<- solve(x)
    getinv <- function() m
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## Write a short comment describing this function

## The function cacheSolve() calculates the inverse of the special "matrix"
## from function makeCacheMatrix().  It also ensures that if the inverse of the
## matrix is present in the cache then skips the calculation and pulls the 
## result from the cache. If the calculation of the inverse is NEW then, it 
## calculates and puts it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinv()
    
    if(!is.null(m)){  ## check if inverse is already in the cache
        message("getting cached data...")
        return(m)
    }
    
    data <- x$get()
    
    ## checking if it is a square matrix and numeric to calculate its inverse
    
    if (nrow(data) == ncol(data)) {  
        if (is.numeric(data)) {
            m <- x$setinv(data)
        } else { print("not a numeric matrix") }
    }
    
    if (nrow(data) != ncol(data)){ 
        print("Not a square matrix") 
    }
    
    m
}
