## R Programming
## Programming Assignment 2
## The following two functions cache the inverse of an invertible matrix 

## makeCacheMatrix creates a special "matrix" that is actually a list containing a functions to perform the following:
   # set the value of the matrix,
   # get the value of the matrix,
   # set the value of the inverse, and
   # get the value of the inverse

makeCacheMatrix <- function(x = matrix()){            # the function takes a matrix (assumed to be invertible) as its argument
  inv <- NULL                                         # create a placeholder variable for the inverse of the matrix
  set <- function(y){                                 # function to set the value of the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x                                 # function to get the value of the matrix
  setinv <- function(inverse) inv <<- inverse         # function to set the value of the inverse
  getinv <- function() inv                            # function to get the value of the inverse
  list(set = set, get = get,                          # specify the names of the functions so they can be called by name after the special matrix is created
       setinv = setinv,
       getinv= getinv)
}


## cacheSolve calculates the inverse of a special "matrix" created by the makeCacheMatrix function

cacheSolve <- function(x,...){                        # the function takes a special "matrix" created by the makeCacheMatrix function as its argument
  inv <- x$getinv()                                   # get the value of the inverse of the matrix
  if(!is.null(inv)){                                  # check to see if the inverse of the matrix has already been calculated; if so, get the inverse from the cache and skip calculation
    message("getting cached data")                    
    return(inv)
  }                                                   # if the inverse of the matrix has not already been calculated:
  data <- x$get()                                     # get the value of the matrix
  inv <-solve(data,...)                               # calculate the inverse of the matrix
  x$setinv(inv)                                       # cache the value of the inverse
  inv
}