## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

## 
## makeCacheMatrix accepts a matrix as input.  If one is not given, then an empty matrix is created and cached.
## This function returns a list of functions that enables one to set the matrix values, retrieve the matrix values, 
## set the inverse of the matrix and retrieve the inverse of the matrix in the environment of the returned object
makeCacheMatrix <- function(x = matrix()) {
    
    # variable to store/cache the inverse of a matrix is initialized to NULL
    matrix_inverse <- NULL  
    
    # function of object to store/cache new values for matrix
    set <-function(y) {    
        x <<- y                     # cache the new matrix in variable x of this functions parent environment
        matrix_inverse <<- NULL     # upon caching new matrix values, set the matrix_inverse to NULL in this functions parent environment
    }
    
    # function to return the cached matrix
    get <- function() x         
    
    # function to cach the inverse of a matrix in matrix_inverse
    setinverse <-function(inverse_input) matrix_inverse <<-inverse_input    
    
    # function to return the cached inverse of a matrix
    getinverse <-function() matrix_inverse          
    
    # list to be returned containing 4 functions: set(), get(), setinverse(), and getinverse()
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    
}



## cacheSolve accepts an object of type makeCacheMatrix
## if the object already has a cached inversed matrix, then this matrix is returned
## if NOT, then this function will calculate the inverse of the matrix cached in the object and cache the inverse.
cacheSolve <- function(x, ...) {
   
    # retrieve the inverse of a matrix cached in the object x and cache it in matrix_inverse
    matrix_inverse <- x$getinverse()    
    
    if(!is.null(matrix_inverse)){ # if matrix_inverse is NOT equal to NULL then execute the following block of code
        
        # print a message stating that the inverse of a matrix is being returned.
        message("getting cached inverse of matrix")  
        
        # returning the inverse of the cached matrix
        return(matrix_inverse)          
    }
    else{  # if the previous if statement evaluated to false, the inverse of the matrix is NULL
        
        # print a message stating that the inverse of a matrix is being cached
        message("setting cached inverse of matrix")  

        # retrieving the cached matrix (NOT THE INVERSE OF THE MATRIX)
        data <- x$get()                 
        
        # calculating the inverse of the matrix retrieved from the previous line and storing it in matrix_inverse
        matrix_inverse <-solve(data, ...)   
        
        # caching the inverse of the matrix
        x$setinverse(matrix_inverse)    
        
        # returning the inverse of the matrix
        return(matrix_inverse)          
    }
}
