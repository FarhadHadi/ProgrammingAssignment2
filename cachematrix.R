## makeCacheMatrix does takes a invertible matrix as input and return a list, which will be used in the second function
## cacheSolve use the list from the first function and return the inverse of the matrix, in addition to look for already calculated inverses 

## makeCacheMatrix return a list containing functions to set and get the matrix aswell as set and get the inverse of the matrices
## The list will be used as the input for the second function called cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  M <- NULL                    
  set <- function(y) {         # set is to set the matrix in environment that differs from current enviroment
    x <<- y
    M <<- NULL
  }
  get <- function() x         # the next three line isto get the rest of the list that will be used in the second function
  setinv <- function(inverse) M <<- inverse 
  getinv <- function() M
  
  list(set=set, get=get, setinv=setinv, getinv=getinv) # returns the list containing set, get, setinv and getinv 
}

## This function calculates the inverse of the matrix, using the list above. In addition, it will check for already calculated inverses

cacheSolve <- function(x, ...) {       # returns a matrix that is the inverse of 'x' through the returned list from makeCacheMatrix
  M <- x$getinv()   
  if (!is.null(M)){                    # if the inverse has already been calculated, return the already calculated M
    message("getting cached data")
    return(M)
  }
  mat.data <- x$get()                  # otherwise calculate the inverse 
  M <- solve(mat.data, ...)
  x$setinv(M)                          # set the inverse in the cache
  
  M
}
