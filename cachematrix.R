## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The key concept to understand in makeCacheMatrix() is that it builds a set of functions and returns 
##the functions within a list to the parent environment. 

makeCacheMatrix <- function(x = matrix()) {
    ##function creates a special "matrix" object that can cache its inverse 
    ##previous line states the formals for the function along with data type
    inv <- NULL
    ##creates empty object that will hold the inverse of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    ##the set function which stores the matrix in an object in the parent environment and sets 
    ##the cached inv object to null when a new matrix is being evaluated
    }
    get <- function() x
    ##gets the data from the parent environment
    setinverse <- function(inverse) inv <<- inverse
    ##sets the inverse of the matrix to the inv object in the parent environment
    getinverse <- function() inv
    ##gets the inverse from the parent environment since it looks there immediately after searching
    ##the current environment
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    ##creates a list object which holds the structure of all of the get, set functions for both the 
    ##data in the matrix and the calculated inverse of the matrix in the parent environment
}


## Write a short comment describing this function
## cacheSolve can access the values of x or inv through the use of getters and setters. This is 
## how cacheSolve() is able to calculate and store the inverse for the input argument if it is of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ##function computes the inverse of the special "matrix" returned by "makeCacheMatrix"
    ##if the inverse has already been calculated (and the matrix has not changed), then "cachesolve"
    ##will retrieve the inverse from the cache 
    ##in previous line matrix x is added to the formals part of the function definition
    inv <- x$getinverse()
    ##calls the getinverse function from list created in "makeCacheMatrix" function
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    ##checks to see if the stored inv value is null and if not it returns the stored value
    }
    data <- x$get()
    ##uses the get function to populate the data variable with the matrix data
    inv <- solve(data, ...)
    ##solves for the inverse of the matrix
    x$setinverse(inv)
    ##sets the inverse of the matrix to the inv object in the parent environment by using the 
    ##set function from "makeCacheMatrix"
    inv
    ##returns the inverse of the matrix
}
