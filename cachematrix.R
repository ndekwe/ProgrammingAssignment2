# This program describes R functions used to cache potentially
# time-consuming computations.
# It is calculating the value of the inverse of the matrix.
 
# The calculation of the inverse for a big matrix may take too 
#long especially if it has to be computed repeatedly.

# If the contents of a matrix are not changing, 
# it makes sense to cache the value of the inverse so 
# that when we need it again, it can be looked up in 
# the cache rather than recomputed. 

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {        # The input of this function is a matrix
        m <- NULL                                  # m is the inverse of the matrix and is initialised to be 
                                                   # NULL everytime makeCacheMatrix is called
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() { x }              # This function returns the value of the given matrix
                       
        setMatrixInverse <- function(solve) m <<- solve   # This function is called by cacheSolve() and will 
                                                          # store the value using <<- operation
        getMatrixInverse <- function() m                  # This function returns the cached value to cacheSolve()
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}


## cacheSolve function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {          #This input is an object  created by makeCacheMatrix function 
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrixInverse()      # Here the function accesses the object 'x' and gets the value of the inverse of the matrix
        if(!is.null(m)) {              #Checking whether the value of the matrix inverse is already cached (not NULL)
                message("getting cached data")   #if yes, this message is displayed
                return(m)            # and the value of the matrix is returned
        }
        data <- x$getMatrix()        # This will be computed if x$getMatrixInverse() is NULL
        m <- solve(data, ...)        # calculation of inverse of the matrix
        x$setMatrixInverse(m)        # storage of the calculated value in x
        m                            # this returns the inverse of the matrix
}

