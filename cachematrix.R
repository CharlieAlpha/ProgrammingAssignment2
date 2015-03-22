## These functions are designed to use the lexical scoping in R
## to solve, store and retrieve the inverse of a matrix. 

## ##Usage Example:
## mm<-matrix(round(10*rnorm(4000000,10),0),2000,2000)
## nm<-makeCacheMatrix(mm)
## cacheSolve(nm)
## ##Now repeat and notice the difference in return time
## cacheSolve(nm)

## This function builds the mechanics of the storage and retrieval 
## with a specialized vector of functions

## This function builds the mechanics of the storage and retrieval 
## with a specialized vector of functions. makeCacheMatrix takes a matrix
## of arbitrary size as its sole argument.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ## Clears the variable M in the local environment
        set <- function(y) {  ##this function passes the argument y to the parent environment
                x <<- y   ##sets the starting matrix in parent environment
                m <<- NULL
        }
        get <- function() x  ## retrieves the starting matrix in parent environment
        setinv <- function(inv) m <<- inv  ## sets the inverted matrix in parent environment
        getinv<- function() m ## retrieves the inverted matrix in parent environment
        list(set = set, get = get,  
             setinv = setinv,
             getinv = getinv)  ## returns vector
}


## This function checks to see if the inverse has already been created.
## If so, the inverse is retrieved, if not the inverse is solved.

cacheSolve <- function(x, ...) {
        m <- x$getinv() ## retrieves the inverse matrix in parent environment
        if(!is.null(m)) {
                message("getting cached data")  ## if the inverse matrix exists, print message
                return(m) ## if the inverse matrix exists, return it
        }
        data <- x$get()  ## if the inverse matrix does not exists, retrieve the starting matrix
        m <- solve(data, ...) ## solve for the inverse of starting matrix
        x$setinv(m) ## store the inverse matrix using the function setinv
        m ## return the inverse matrix
}


