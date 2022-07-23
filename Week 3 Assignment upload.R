# This function takes a matrix as an input (default is a null matrix).
# This function is capable of caching the matrix and the inverse if desired,
# as well as retrieving both.

makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    mx <- NULL
    
    # cache the matrix, set inverse to NULL.
    cache_mx <- function(m) {
        mx <<- m
        inv <<- NULL
    }
    
    # create the inverse matrix, cache it and the original matrix.
    inv_mx <- function(m) {
        inv <<- solve(m)
        mx <<- m
    }
    
    # Retrieve the cached inverse.
    get_inv <- function() inv
    
    # Retrieve the cached matrix.
    get_mx <- function() mx
    
    # Return the list of available methods.
    list(inv_mx = inv_mx, get_inv = get_inv,
         cache_mx = cache_mx, get_mx = get_mx)
}


# This function will take an object that was used in the previous function as input.
# It will check if the inverse of the matrix has already been produced and cached.
# If it has been cached already then it will be returned, if not then it will be
# created, cached and returned.
cacheSolve <- function(x, ...) {
    
    # retrieve the cached inverse.
    i <- x$get_inv()
    
    # if it exists, retrieve it.
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    
    # If no inverse then generate it and display it
    message("generating inverse")
    
    # Retrieve the stored matrix
    m <- x$get_mx()
    
    # Create and store the inverse
    x$inv_mx(m)
    
    # Retrieve the inverse
    x$get_inv()
}
