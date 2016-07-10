## Programming Assignment 2: Lexical Scoping
## Author: Ivan
## Date: 10/7/2016
## Description: 2 functions that can be used to retrive inverse of matrix efficiently as cache

## Create a list which holds an invertible square matrix and its inverse (cache) 

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseMatrix) im <<- inverseMatrix
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Get inverse matrix from the list created from makeCacheMatrix
## If inverse matrix is existed in the list (cache), return the cached value.
## Otherwise, compute the inverse and save it to the cache in the same list.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if(!is.null(im)) {
                message("Getting cached data")
                return(im)
        }
        data <- x$get()
        message("Computing inverse")
        im <- solve(data, ...)
        message("Saving to cache")
        x$setinverse(im)
        im
}