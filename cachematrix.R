###############################################################################
###############################################################################
##
## NAME:    makeCacheMatrix
##
##    DESCRIPTION:      Creates a "special matrix", a list containing a matrix
##                      and commands for working with it.
##
##    PARAMETERS:       X     An ordinary square matrix
##
##    RETURNS:          The "special matrix" list.
##
###############################################################################
###############################################################################
makeCacheMatrix <- function ( x = matrix() ) {
      
      ##    This is where the inverse matrix will be stored.
      ##    Initialized as NULL so we know it hasn't been calculated yet.
      x_inv <- NULL
      
      
      ##    Updates the matrix and clears the cache.
      set <- function(y) {
            x <<- y
            x_inv <<- NULL
      }
      
      
      ##    Returns the matrix.
      get <- function() {
            return(x)
      }
      
      
      ##    Returns the stored inverse.
      getCache <- function() {
            return(x_inv)
      }
      
      
      ##    Sets the inverse.
      setCache <- function(y) {
            x_inv <<- y
      }
      
      
      ##    Returns the "special matrix" list.
      list( set = set, get = get, setCache = setCache, getCache = getCache )
      
}


###############################################################################
###############################################################################
##
## NAME:    cacheSolve
##
##    DESCRIPTION:      Returns the inverse of a given "special matrix",
##                      pulling the value from the matrix's cache if possible.
##
##    PARAMETERS:       X     A "special matrix"
##
##    RETURNS:          The inverse of the square matrix inside x.
##
###############################################################################
###############################################################################
cacheSolve <- function(x) {
      
      ##    Checks if the cached value is null,
      ##    which means it needs to be calculated.
      if( is.null(x$getCache()) ) {
            
            ##    Pulls the matrix from the list, calculates its inverse,
            ##    and stores the result.
            x$setCache(solve(x$get()))
            
      ##    If the cache exists, outputs a message informing the user
      ##    instead of performing any calculations.
      } else {
            message("using cache")
      }
      
      ##    Returns the inverse.
      return(x$getCache())
      
}








