# The two functions below can cache the inverse of a matrix (assuming the
# supplied matrix is always invertible). If the inverse of the matrix is
# already computed, then the inversed matrix is retrieved from the cache.
# If the matrix has changed or not yet computed, the inverse of that matrix
# have to be computed.

# This function creates a special "matrix" object that can cache its inverse
# makeCacheMatrix takes an invertible matrix 'm' as only one argument
makeCacheMatrix <- function(m = matrix()) {
    # Initialization of the variable 'sm' which will contain the
    # inversed matrix
    sm <- NULL
    # Store the new matrix 'cm' in 'm' in the SET's parent environment
    # (global one) and reset the inversed matrix to 'NULL' (if different matrix then 
    # inversed matrix will be different too)
    set <- function(cm) {
        m <<- cm
        sm <<- NULL
    }
    # Retrieve the matrix 'm'
    get <- function() m
    # Store the inversed matrix of 'm' in 'sm' in the SETINVERSEDMATRIX's
    #parent environment (global one)
    setInversedMatrix <- function(solve) sm <<- solve
    # Retrieve the reversed matrix through 'sm' which will contain the result
    # of 'solve(m)' function
    getInversedMatrix <- function() sm
    # Create a list of the functions contained in makeCacheMatrix function
    # for using it later on
    list(set = set, get = get, 
         setInversedMatrix = setInversedMatrix, 
         getInversedMatrix = getInversedMatrix)
}

# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix
# cacheSolve takes the special matrix 'm' returned by the function
# makeCacheMatrix as argument
cacheSolve <- function(m, ...) {
    # Store the inversed matrix of 'm' using the GETINVERSEDMATRIX function
    # of the matrix (object) 'm'
    sm <- m$getInversedMatrix()
    # If the inversed matrix of 'm' was already computed, we get the result
    # and print it out
    if (!is.null(sm)) {
        message("Getting Cached Data !")
        return(sm)
    }
    # Else, we have to get the matrix 'm' passed to the function cacheSolve
    matrix_to_solve <- m$get()
    # And we compute the inversed matrix of 'm' through 'solve()' function
    sm <- solve(matrix_to_solve)
    # Finally, we store the inversed matrix of 'm' in the SETINVERSEDMATRIX
    # function...
    m$setInversedMatrix(sm)
    # ... and printing it out
    sm
}
