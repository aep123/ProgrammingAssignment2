## These two functions work together to compute the inverse of a matrix 
## and store the inverse in a cache.

## makeCachematrix creates a list of functions that are responsible
## for storing and retrieving the matrix and its inverse from a cache

makeCacheMatrix <- function(current_mat = matrix()) {

    # begin by setting inv_mat to NULL so the function works when run
    # for the first time
    
    inv_mat <- NULL
    
    ## then create a series of functions to load the matrix and its inverse
    
    ## first create a function to load a matrix into a cache (current_mat) and  
    ## initialize a cache to hold its inverse (inv_mat). Both caches exist in
    ## the makeCacheMatrix frame.
    
    set_mat <- function(c) {
        
        current_mat <<- c
        inv_mat <<- NULL
    }
    
    ## this function returns the matrix currently cached in the makeCacheMatrix
    ## frame
    
    get_mat <- function() {
        
        current_mat
    }
    
    ## this function takes the inverse matrix that is passed into it  
    ## and caches it as inv_mat in the parent makeCacheMatrix frame
    
    set_inv <- function(i = matrix()){
        
        inv_mat <<- i
    }
    
    ## this function returns the inverse matrix that is stored in the 
    ## makeCacheMatrix frame
    
    get_inv <- function() {
        
        inv_mat
    }
        
    ## return the output of each function created above as a list 
    
    list(set_mat = set_mat, get_mat = get_mat, set_inv = set_inv,
         get_inv = get_inv)
}


## This function uses the functions created in makeCacheMatrix to take
## a matrix and return an inverse of that matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    ## evalute the matrix passed into cacheSolve by retrieving its inverse
    ## from the makeCacheMatrix frame associated with the function
    
    inv_mat <-x$get_inv()
 
    ## if makeCacheMatrix has already cached a computed inverse for the passed 
    ## matrix, indicate the inverse is being retrieved from a cache and return it
    
    if(!is.null(inv_mat)) {
        
        message("getting cached inverse")
        return(inv_mat)
    }
    
    ## if the inverse of the passed matrix is NULL, compute the inverse,
    ## and pass it into makeCacheMatrix to assign it to the frame
    ## for the matrix that is being evaluated here
    
    else {
        local_mat <- x$get_mat()
        inv_mat <- solve(local_mat)
        
        x$set_inv(inv_mat)
        
    ## and return the inverse as a result of this function with a message
    ## that shows the function is computing the inverse (vice retrieving a
    ## cached value for the inverse)
        
        message ("solving the matrix now")
        inv_mat
    }
}
