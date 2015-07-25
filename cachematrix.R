#------------------------------------------------------------------------------
#
#  cachematrix.R
#  
#  20150725 BGH; iaugmented with function definitions.
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

#------------------------------------------------------------------------------
#
#  makeCacheMatrix: This function creates a special "matrix" object that can
#  cache its inverse.
#
#------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {

        solved <- NULL
        set <- function(y) {
                x <<- y
                solved <<- NULL
        }
        get <- function() x
        setsolved <- function(solvedGiven) solved <<- solvedGiven
        getsolved <- function() solved
        list(set = set, get = get,
             setsolved = setsolved,
             getsolved = getsolved)

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

        solution <- x$getsolved()
        if(!is.null(solution)) {
                message("getting cached data")
                return(solution)
        }
        data <- x$get()
        solution <- solve(data, ...)
        x$setsolved(solution)
        solution

}

#------------------------------------------------------------------------------
