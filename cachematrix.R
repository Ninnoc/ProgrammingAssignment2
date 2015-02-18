#### Pair of functions to cache and retrieve the computation for inverse of square matrix ####


## Create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { # Function to create the special matrix, argument "x" is
    # the matrix provided when this function is called
    
    Inv <- NULL # Set Inv to NULL as a placeholder
    
    set <- function(NewMatrix) { # Function to give x new matrix values and reset Inv
        x <<- NewMatrix # Assigns new matrix values to x
        Inv <<- NULL # Resets Inv to NULL so it has to be calculated
    }
    
    get <- function() x # Return x
    
    setsolve <- function(solve) Inv <<- solve # Solve x  
    
    getsolve <- function() Inv # Return either the calculated Inv from setsolve, or
    # the NULL value if Inv has been reset
    
    list(get = get, 
         setsolve = setsolve,
         getsolve = getsolve) # Create a list that includes labels for the above 4 functions,
    # to refer to them in cacheSolve
}


## Compute inverse of the special matrix returned by makeCacheMatrix above
## If already calculated (and matrix not changed), then retrieve from cache

cacheSolve <- function(x, ...) { 
    
    Inv <- x$getsolve() # Take Inv from getsolve function in makeCacheMatrix
    
    if(!is.null(Inv)) { # If Inv is not NULL that means Inv for this matrix is already calculated
        # and available in cache
        
        message("getting cached data") # Let the user know data will be retrieved from cache
        
        return(Inv) # Return the cached Inv
    }
    data <- x$get() # If Inv is NULL, that means the matrix is new/changed and Inv has been reset,
    # therefore Inv needs to be calculated. First we assign new x to data.
    
    Inv <- solve(data, ...) # 
    
    x$setsolve(Inv)
    
    Inv
}


## Now a little test

TestMatrix <- makeCacheMatrix(matrix(1:4, 2, 2)) # Creates a new matrix with makeCacheMatrix function
# Inv value will be NULL as cacheSolve function
# has not been called yet
cacheSolve(TestMatrix) # Calculate the Inverse for the first time
cacheSolve(TestMatrix) # Get Inverse from cache