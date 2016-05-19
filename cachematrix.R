## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. 
## The purpose of this R script is to have a pair of functions that cache the 
## inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## function to set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## function to get the value of the matrix
        get <- function() x
        ## function to set the value of the inverse of matrix
        setinverse <- function(inverse) inv <<- inverse
        ## function to get the value of the inverse of matrix
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. Otherwise, it 
## computes the inverse and
## sets the inverse in the cache via setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get inverse from cache via getinverse function in makeCacheMatrix
        inv <- x$getinverse()
        
        ## Check if inverse from cache is NOT NULL
        ## If Yes - Getting incerse from cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## If inverse from cache is NULL
        ## get the matrix
        message("compute inverse")
        matData <- x$get()
        ## Computes inverse of square matrix using solve function
        inv <- solve(matData, ...)
        ## Set the computed inverse to cache and return the inverse
        x$setinverse(inv)
        inv
}

## Testing:
## > source("cachematrix.R")
## Create a square matrix
## > x<-matrix(c(1,2,2,1),nrow=2,byrow=TRUE)

## Call makeCacheMatrix function to create the matrix object and list of functions
## > im = makeCacheMatrix(x)
## > im$get()
##      [,1] [,2]
## [1,]    1    2
## [2,]    2    1

## Call cacheSolve function to compute inverse of matrix
## 1st call the inverse is not available from cache.
## Message "compute inverse" was displayed.
## The inverse of matrix is computed and set in cache.
## compute inverse
## [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333

## Call cacheSolve function again to compute inverse of matrix
## 2nd call the inverse is available from cache.
## Message "getting cached data" was displayed.
## The inverse of matrix is getting from cache.
## > cacheSolve(im)
## getting cached data
##            [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
