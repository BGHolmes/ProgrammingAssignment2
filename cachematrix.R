#------------------------------------------------------------------------------
#
#  cachematrix.R
#  
#  20150725 BGH; augmented with function definitions, as assigned.
#
#------------------------------------------------------------------------------
#
#  Matrix inversion is usually a costly computation and there may be some
#  benefit to caching the inverse of a matrix rather than computing it
#  repeatedly (there are also alternatives to matrix inversion that we will not
#  discuss here). Your assignment is to write a pair of functions that cache
#  the inverse of a matrix.
#
#  Write the following functions:
#  
#	makeCacheMatrix
#	cacheSolve
#
#  Computing the inverse of a square matrix can be done with the solve function
#  in R. For example, if X is a square invertible matrix, then solve(X) returns
#  its inverse.
#  
#  For this assignment, assume that the matrix supplied is always invertible.
#
#------------------------------------------------------------------------------
#
#  Example use:
#
#	> source( "cachematrix.R" )
#	> Matrix <- matrix( stats::rnorm(16),ncol=4,nrow=4 )
#	> InverseDirect <- solve(Matrix)
#	> inverseCalc <- makeCacheMatrix(Matrix)
#	> InverseCached <- cacheSolve(inverseCalc)
#	> InverseReused <- cacheSolve(inverseCalc)
#	getting cached data
#	> all(InverseDirect==InverseCached)
#	[1] TRUE
#	> all(InverseDirect==InverseReused)
#	[1] TRUE
#	> all(Matrix == InverseReused)
#	[1] FALSE
#	> 
#
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#
#  makeCacheMatrix: This function creates a special "matrix" object that can
#  cache its inverse.
#
#------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {

        inverseCached <- NULL
        set <- function(y) {
                x <<- y
                inverseCached <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseGiven) inverseCached <<- inverseGiven
        getinverse <- function() inverseCached
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

#------------------------------------------------------------------------------
#
#  cacheSolve: This function computes the inverse of the special "matrix"
#  returned by makeCacheMatrix above. If the inverse has already been
#  calculated (and the matrix has not changed), then cacheSolve should retrieve
#  the inverse from the cache.
#
#------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {

        result <- x$getinverse()
        if(!is.null(result)) {
                message("getting cached data")
                return(result)
        }
        data <- x$get()
        result <- solve(data, ...)
        x$setinverse(result)
        result

}

#------------------------------------------------------------------------------
