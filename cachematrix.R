## makeCacheMatrix and cacheSolve work together to 
     ## A) find the inverse of a square matrix,
     ## B) hold that calculated inverse matrix in a cache,
     ## C) report that inverse matrix,
     ## D) checking first that the inverse matrix doesn't need to be recalculated,
     ## E) and if it does, then recalculating it.

## makeCacheMatrix requires the user to input a square matrix, and will create an object
## to hold the matrix, the inverse of the matrix, and functions to re-set the original matrix.

## cacheSolve does the actual math to make the inverse matrix, and checks the cache to see
## if this needs to be recalculated before reporting either the inverse matrix cached or a 
## new calculation of the inverse matrix.





####### makeCacheMatrix #########
# This function takes an invertible matrix as the input, and creates a list object containing elements
# for the set() function to set a new matrix input, the get() function to retrieve the input matrix,
# the setInv function to later solve for the inverse of the input matrix, and the getInv() function to
# retrieve this inverse matrix later from a cache stored in the object "m".
# makeCacheMatrix is designed to work in series with cacheSolve, below.
# makeCacheMatrix assumes the user will only supply a matrix that in invertible.
makeCacheMatrix <- function(x = matrix()) {
     # make an empty object to hold the inverse of the matrix
     m <- NULL
     
     # Make a function to allow changing of the matrix after the first makeCacheMatrix is called.
     set <- function(y) {
                x <<- y
                m <<- NULL
     }
     
     # Define a function to retrieve the original matrix
     get <- function() x
     
     # Define a function that finds the inverse of the matrix and 
     # assigns that inverse matrix a symbol
     setInv <- function(solve) m <<- solve
     
     # Define a function to retrieve the inverse matrix
     getInv <- function() m
     
     # Output a list of all functions in this function
     list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


####### cacheSolve #########
# This function A) tries to check if makeCacheMatrix has calculated and cached a matrix, and if
# that matrix has changed, and then B) retrieves that cached matrix if it hasn't changed and 
# C) will create the inverse of the input matrix if the cached matrix has changed.

cacheSolve <- function(x, ...) {
     m <- x$getInv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInv(m)
     m
}


##### Examples of the functions in action #####
# Uncomment to run.
# test <- matrix(data=rnorm(25), nrow = 5, ncol = 5)
# aa <- makeCacheMatrix(test)
# cacheSolve(aa)
# cacheSolve(aa)
# 
# test2 <- matrix(data=rnorm(25), nrow = 5, ncol = 5)
# aa$set(test2)
# cacheSolve(aa)
# cacheSolve(aa)
