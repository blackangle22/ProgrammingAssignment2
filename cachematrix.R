
## setwd() where your project contains
## add matlib package

## This function set the matrix
makeCacheMatrix <- function(x = matrix()) {  #Get the matrix 
  s <- NULL    # set the vector
  set <- function(y) {   #set the function which will assign the matrix to another variable
    x <<- y
    s <<- NULL
  }
  get <- function() x   #get the matrix 
  setinverse <- function(inverse) s <<- inverse    #set the inverse using inverse function from matlib
  getinverse <- function() s     # get the inverse
  list(set = set, get = get,      #set the list
       setinverse = setinverse,
       getinverse = getinverse)
}
##
## Get the inverse
cacheSolve <- function(x, ...) {
  s <- x$getinverse()    #get the inverse using the function
  if(!is.null(s)) {     # if inverse is not null return the s 
    message("getting inversed matrix")
    return(s)
  } 
  data <- x$get()    #get the matrix 
  s <- solve(data, ...)   #solve the data
  x$setinverse(s)    #setting the inverse
  s    #return the inverse or data
}





#### Output-
## source("cachematrix.R")
## > B <- matrix(c(1,2,3,4),2,2)
##> B1 <- makeCacheMatrix(B)
##> B1$get()
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(B1)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
