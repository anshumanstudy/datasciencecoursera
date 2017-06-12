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
    setinv <- function(inv) m <<- solve(x)
    getinv <- function() m
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinv()
    
    if(!is.null(m)){
        message("getting cached data...")
        return(m)
    }
    
    data <- x$get()
    
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
