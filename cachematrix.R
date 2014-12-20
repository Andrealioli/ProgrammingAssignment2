##This Function stores the matrix 'x' that will
##be used in the function that will calculate the inverse 
##of the matrix (cacheSolve)

makeCacheMatrix <- function(x = matrix()) { 
## inverse is the variable created to store the inverse of a matrix
        inverse <-  NULL 
        set <- function(matrix) {
                x <<- matrix
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        

}


## This function will return a matrix that is the inverse of 'x'
## cacheSolve gets the inverse of 'x' if it has been alredy calculated and 
## stored on function makeCacheMatrix 
## if not it will calculete the the inverse of 'x' and return the result


cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        
        
    
        if(!is.null(inverse)) {
               
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
        
}
