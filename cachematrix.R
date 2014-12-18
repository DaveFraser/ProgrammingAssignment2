makeCacheMatrix <- function(x = matrix()) {      # input x will be a matrix
    
    inv <- NULL    #  inv is the inverse and it is reset to NULL each time that makeCacheMatrix is called
    
    set <- function(y) {  # Takes an input matrix
        x <<- y           # Saves the input matrix
        inv <<- NULL      # Resets the inverse to NULL
    }
    
    get <- function() { x }   # This function returns the value of the original matrix
    
    setInverse <- function(solve)  { inv <<- solve }    #  Called by cacheSolve() during the first cacheSolve()
        
    getInverse <- function() { inv }   # Returns the cached value to cacheSolve() on subsequent accesses
    
    list(get = get,               #   Accessed each time makeCacheMatrix() is called   
        setInverse = setInverse,  #   It is a list of the internal functions, so the calling
        getInverse = getInverse)  #   function knows how to access those methods
}


cacheSolve <- function(x, ...) {    # The input x is an object created by makeCacheMatrix
    inv <- x$getInverse()           # it accesses the object 'x' and inverts it if the inverse was not already cached
    if(!is.null(inv)) {              
        
        message("getting cached data")  # Echo to console if cached inverse exists
        return(inv)                     # Return the inverse and end the function cacheSolve
                                        
    }
   
    data <- x$get()           # This code only used if x$getInverse() returned NULL
    inv <- solve(data, ...)   # If inv == NULL then it will calculate the inverse
    x$setInverse(inv)         # Store the calculated inverse value in x
    inv                       # Return the inverse to the code that called this function
}