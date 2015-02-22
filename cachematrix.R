## makeCacheMatrix creates a special matrix object that can cache its inverse
## cacheSolve returns the inverse of the original matrix passed by either
### calculating it using the solve function, or
### retrieving it from cache (if it has been calculated previously)


## Input argument for makeCacheMatrix is a rectangular matrix 
## (supposed to be invertible)

makeCacheMatrix <- function(x = matrix()) {
    ## Create a special matrix object that can store its inverse
  
        m <- NULL
        
    ## Store the original matrix
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
    ## Get the original matrix
        get <- function() x
    
    ## Store the inverse in cache
        setinv <- function(inv) m <<- inv
    
    ## Get the inverse from cache
        getinv <- function() m
    
    ## Return a list of the above functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Input argument for cacheSolve is the special matrix object
## created by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Get the inverse of the matrix from cache
        m <- x$getinv()
        
    ## If the inverse retrieved from cache is not empty, then 
    ## return the inverse and exit from function
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
    
    ## Otherwise, calculate the inverse from original matrix
        data <- x$get()
        m <- solve(data, ...)
    
    ## Push the inverse to cache for further use
        x$setinv(m)
    
    ## ...and return the inverse
        m
}