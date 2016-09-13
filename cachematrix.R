## Comment descriptions modified from readme file to fit this scripts' purpose.
## This scipt uses the `<<-` operator to assign a value to an object in an 
## other than current environment. The two functions below are used to create a
## special object that stores a matrix and caches the inverse of the matrix.


## The first function, makeCacheMatrix creates a special "maxtix", which is
## really a list containing functions to:
## 
## 1.  set the value of the maxtix
## 2.  get the value of the maxtix
## 3.  set the value of the inverted matrix
## 4.  get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix(data=,2,2)) {

        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse
        
        getinverse <- function() i
        
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix"
## created with the makeCacheMatrix function. It first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        
        if(!is.null(i)) {
        message("getting cached data")
        return(i)
        }
        
        data <- x$get()
        
        i <- solve(data,...)
        
        x$setinverse(i)
        
        i
}


