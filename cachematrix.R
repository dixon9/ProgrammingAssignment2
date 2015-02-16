##  Programming Assignment #2
##  Coursera R Programming course
##  Program:  cachematrix.R
##  cachematrix.R includes two functions
##  makeCacheMatrix() creates a matrix that caches its inverse
##  cacheSolve() computes the inverse of the matrix defined by makeCacheMatrix()
##    if the inverse is already calculated, cacheSolve() returns inverse from cache
##  Code adapted from makeVector() and cachemean() sample functions


## Create matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
       #  Argument x is a square matrix whose inverse exists
        m <- NULL  #  Assigns m the value NULL in the local environment
        set <- function(y) {
                x <<- y  #  Assigns the  x the value of the argument y in the parent environment
                m <<- NULL  #  Assigns m the value NULL in the parent environment -- if set is called, m is NULL
        }
        get <- function() x  #  If get() is called, the value of x the numeric vector is returned
        setinvmat <- function(vm) {
                m <<- vm  #  If setinvmat() is called, m is assigned the value of argument vm from the parent environment
        }
        getinvmat <- function() m  #  If getinvmat() is called, m is returned
        list(set = set, get = get,  #  Puts all 4 functions in a list; returns the list
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}


## Return the inverse of square matrix 'x' supplied as argument to makeCacheMatrix, cached version returned if available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #  cachemean() returns mean of vector created above in makeVector
        #  if mean was already calculated, it returns the cached mean
        #  if mean wasn't already calculated, this function will calculate and return it
        #  argument x is the vector created by makeVector()   
        m <- x$getinvmat()  #  Assigns the value of m from makeVector function to q; either NULL or non-NULL
        if(!is.null(m)) {
                message("getting cached data")
                return(m)  #  If q is not null, i.e., mean already exists, q is returned along with text
        }
        data <- x$get()  #  Gets the data, i.e., the x argument to makeVector(x)
        m <- solve(data, ...)  #  Calculates the mean of the x argument to makeVector(x)
        x$setinvmat(m)  #  This is the cache step; assigns the value of q to m
        m  #  Returns m
}
