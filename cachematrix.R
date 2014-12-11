## These functions allow creation of a object containing a matrix and its inverse 
## that can cache the inverse to save time if multiple inverse calculations are required

## Create a cache matrix object with methods to set and get the matrix,
## and set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) m <<- inverse
    
    getinverse <- function() m

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## Return the inverse of a cache matrix object, using the cached copy if available
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")        
        return(m)
    }
    data_ <- x$get()
    m <- solve(data_)
    x$setinverse(m)
    m    
    
}
