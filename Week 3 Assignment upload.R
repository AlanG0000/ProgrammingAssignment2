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
