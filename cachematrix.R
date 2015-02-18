##  Programming Assignment #2
##  Coursera R Programming course
##  Program:  cachematrix.R
##  cachematrix.R includes two functions to compute and return the inverse of invertible square matrix x
##  makeCacheMatrix(x) takes an invertible square matrix x and 
##    returns a list of 4 functions to set the input matrix, get the input matrix,
##    cache the value of the computed inverse matrix, and return the value of the inverse matrix
##  cacheSolve(x) takes a matrix processed by makeCacheMatrix(x) 
##  The first time is cacheSolve(x), the inverse matrix is computed using R function solve(x, ...)
##  The computed inverse matrix is stored for subsequent function calls
##  If cacheSolve(x) is called again and the input matrix is not re-input via x$set()
##    then the cached inverse matrix is returned
##  Code adapted from makeVector() and cachemean() sample functions


##  Create a list of functions to set and get an invertible square matrix and to cache and return its inverse
makeCacheMatrix <- function(x = matrix()) {
        ##  Argument x is a square matrix whose inverse exists
        ##  Assigns m the value NULL in the local environment
        m <- NULL  
        ##  set() assigns x the value of square matrix y and sets m to NULL, both in the parent environment 
        set <- function(y = matrix()) {
                x <<- y  
                m <<- NULL
        }
        ##  get() returns the value of square matrix x, which will subsequently have its inverse computed
        get <- function() x
        ##  setinvmat() assigns m the value of vm, the cached inverse matrix of x
        setinvmat <- function(vm) {
                m <<- vm          
        }
        ##  getinvmat() returns the value of m, the inverse matrix of x
        getinvmat <- function() m
        ##  Return the four functions as a list
        list(set = set, get = get, 
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}


##  Return the inverse of invertible square matrix y supplied as argument to makeCacheMatrix(y)
##  Cached inverse matrix returned if available

cacheSolve <- function(x, ...) {
        ##  Argument is object "makeCachematrix(y)" or object "x$set(z)", 
        ##    where z is a "new" invertible square matrix whose inverse we wish to compute
        ##  If the inverse of y was already computed and cached and if y is not reset to z,
        ##    then the cached version of the inverse of y is returned (saves on computation time)
        ##  If the inverse of y was not previously computed, or if y is reset to z via x$set(z), 
        ##    then the inverse of y is computed, cached for subsequent calls of cacheSolve(x, ...), and returned
        ##  Assign local variable m the current value of the inverse matrix of y, 
        ##    m will be NULL if never computed or if y is reset
        m <- x$getinvmat()
        ##  If m is not NULL, it is returned
        if(!is.null(m)) {
                message("getting cached data")
                return(m)  #  If q is not null, i.e., mean already exists, q is returned along with text
        }
        ##  Otherwise, data is assigned value of y, m is assigned value of computed inverse of y
        data <- x$get()
        m <- solve(data, ...)
        ##  Computed inverse, m, is cached by calling x$setinvmat(m) and then returned
        ##  If cacheSolve(x, ...) is called again and y is not reset, this cached inverse is returned
        x$setinvmat(m)
        m
}
