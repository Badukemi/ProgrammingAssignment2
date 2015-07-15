## Functions for R Programming assignment 2
## code based on example makeVector and cachemean by R Peng
## but done with a lot of head scratching to figure out how it worked and
## how tocall it!

## ____________________________________________________
## This function will take a matrix and store it while offering functions to get
## the inverse.
## call as variable<-makeCacheMatrix(newdata) where newdata is a matrix
## defined using the matrix() function e.g. matrix(1:4,2,2) either directly
## or in a separate variable.
## It creates four operators on the variable:
## view the contents using variable$get()
## set new values using variable$set(data)
## calculate and cache the inverse using variable$setinv (only internal)
## return the inverse using variable$getinv  (only internal)

makeCacheMatrix <- function(x = matrix()) { #name matrix
    m <- NULL               # clear the cache
    set <- function(y) {    # if $set is selected 
        x <<- y             # make that the new matrix
        m <<- NULL          # and clear the cache
    }
    get <- function() x     # if $get is selected return the value of matrix
    setinv <- function(solve) m <<- solve   # solve the inverese and cache it
    getinv <- function() m  # if $getinv is selected return the cache
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)   #list of function options
}

##__________________________________________________________________________
## This function checks to see if the inverse already exists and returns that or
## calculates a new one and caches it in m
## call it by usinge cacheSolve(variable)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()                         #check to see if the inverse 
    if(!is.null(m)) {                       #already exists
        message("getting cached data")      #if the inverse exists 
        return(m)                           #return the saved one and exit
    }
    data <- x$get()                         #put the matrix in a temp variable
    m <- solve(data, ...)                   #get the inverse and assign to m
    x$setinv(m)                             #cache the inverse
    m                                       #return the inverse
}
