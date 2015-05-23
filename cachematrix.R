## Calculate inverse of matrix. If inverse calculated previously,
## obtain value from cache to reduce computation time.

## Function to make the matrix and create the necessary
## getter and setter functions for the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    # setter function
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # getter function
    get <- function() {
        return(x)
    }
    
    # setter function for inverse of matrix
    setinverse <- function(inverse) {
        i <<- inverse
    }
    
    # getter function for inverse of matrix
    getinverse <- function() {
        return(i)
    }
    
    # create list of functions 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    
}


## Function to calculate inverse of matrix, either by retrieving
## cached value or by computing inverse for new matrix.
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    # check if cache is empty
    if(is.null(i)) {
        # compute and return inverse
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
    }
    else {        
        # if inverse has been cached for SAME matrix, retrieve value
        message("getting cached data")
    }
        
    return(i)
}
