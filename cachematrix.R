## The below two functions makeCacheMatrix and cacheSolve will cache the inverse of a matrix.


## The function "makeCacheMatrix" creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize a variable for inverse function
        m <- NULL
        
        ## Set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Get the matrix
        get <- function() x                                     ## Return the matrix
        
        ## Set inverse of the matrix
        setinverse <- function(inverse) m <<- inverse 
        
        ## Get inverse of the matrix
        getinverse <- function() m                              ## Return the inverse
        
        ## Returning a list of above actions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()                             ## Return the matrix that is inverse of 'x'
        
        ## Getting the cace data
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()                                 ## Subseting for the matrix
        
        m <- solve(data, ...)                           ## Calculating Inverse
        
        x$setinverse(m)                                 ## Set the Inverse for 'x'
        
        m                                               ## Return a matrix that is the inverse of 'x'
        
        
}
