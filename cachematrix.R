## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix sets up some functions that are subsequently called by the function cacheSolve
##  "m" is set to Null every time makeCacheMatrix is called
##  get returns the value of the original (non-inverse) matrix
##  setsolve is called by cacheSolve...this creates the matrix's inverse and does a superassignment (so it does not need to be recalculated)
##  getsolve returns the superassigned value to cacheSolve 
##  Written - August 23, 2014 

makeCacheMatrix <- function(x = matrix()) {

m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function actually creates the inverse of the Matrix (if it has not been created before) and 
## makes a permanent copy so it does not have to be re-caculated each time.
## The x$getsolve gets the superassgined inverse matrix m (if it exists)
## If it ALREADY exists, the if logic prints "getting cached inverse matrix" and just returns the m (without recalculating)
## If it does NOT exist, the x$get function gets the original matrix, puts it in the solve(data...) function, and puts it in m
## THEN it uses x$setsolve to superassign the m (inversed matrix), so it now does not need to be recalculated
## Written - August 23, 2014

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getsolve()
      if(!is.null(m)) {
             message("getting cached inverse matrix")
             return(m)
      }
      data <- x$get()
      m <- solve(data,...)
      x$setsolve(m)
      m

}
