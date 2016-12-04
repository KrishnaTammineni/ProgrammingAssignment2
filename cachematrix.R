## Put comments here that give an overall description of what your
## functions do

## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation 
## It is beneficial to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


makeCacheMatrix <- function(x = matrix()) {
## set the value of inverse matrix to NULL
         inver <- NULL
## set the value of matrix
 +  set <- function(y) {
 +    x <<- y
 +    inver <<- NULL
 +  }
## get the value of matrix
 +  get <- function() x
## set the inverse value of a given matrix 
 +  setInverse <- function(inverse) inver <<- inverse
## get the inverse value of a given matrix 
 +  getInverse <- function() inver
 +  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

        
## This function computes the inverse of the special "matrix" created by  makeCacheMatrix above.
## If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## getInverse of the matrix
        inver <- x$getInverse()
       ## check if the inverse is created or not
        if (!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        mat <- x$get()
        inver <- solve(mat, ...)
        x$setInverse(inver)
        inver
}
