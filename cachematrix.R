## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Given a matrix, The function creates a list:
# set; is a function to reassign the matrix, it resets the Inverse to "NULL"
# get; return the matrix 
# SetInverse; is used by the cacheSolve function to save the Inverse matrix
# GetInverse; returns the Inverse matrix saved in cache 
 
makeCacheMatrix <- function(x = matrix()) {

                I <- NULL
                set <- function(y) {
                        x <<- y
                        I <<- NULL
                }
                get <- function() x
                setInverse <- function(Inverse) I <<- Inverse 
                getInverse <- function() I
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        
}


## Write a short comment describing this function
#Given a list create with MakeCacheMatrix, the function either calculates the Inverse matrix or return the value if
#previously calculated:
# if x$getInverse is not NULL then the Inverse matrix has been calculated and is returned
# solve(data,) calculated the inverse of the matrix  
# x$SetInverse; save the Inverse matrix to the list


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        #Calculate the Inverse if it has not already been calculated
        data <- x$get() #Load the Matrix
        I <- solve(data, ...)
        x$setInverse(I)
        I
}
