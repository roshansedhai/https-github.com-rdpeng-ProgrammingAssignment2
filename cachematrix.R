makeCacheMatrix <- function( m = matrix() ) {

	## Initialize the inverse property
    n <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            n <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	m
    }

    ## Inverse of the matrix
    setInvmatrix <- function(Invmatrix) {
        n <<- Invmatrix
    }

    ## Get the inverse of the matrix
    getInverse <- function() {
        n
    }

    ## list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}
