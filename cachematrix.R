@ -1,15 +1,87 @@
## Put comments here that give an overall description of what your
## functions do
##R Programming -Assignment 2


## The purpose of this assignment is to work 
##on caching so that computation operations can be
##done for the first time and results are cached for future use.
##Cbe two functions shown here help in caching the

## Below 2 functions i.e makeCacheMatrix and cacheSolve
##will take a matrix,compute its inverse and cache the result.
#Any future recomputation of the inverse of the same matrix
#will just get the cached result and save from re-computation.


## makeCacheMatrix Function:
## makeCacheMatrix will take matrix value as an input.
## There will be inside makeCacheMatrix function other 
##functions for setting the matrix,getting the matrix,
## setting the inverse of the matrix and getting the inverse
## of the matrix


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  
  # set initial to NULL
 
  MatVal <- NULL
  
  # set function
  # Sets the matrix values
  set <- function(y) {
    x <<- y
    MatVal <<- NULL
  }
  
  # get function
  # Gets the matrix values
  get <- function() x
  
  # set function
  # Sets the inverse of the matrix
  setinv <- function(Matinv) MatVal <<- Matinv
  
  # get function
  # Gets the inverse of the matrix
  getinv <- function() MatVal
  
  # Create a list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)	
}

## cacheSolve:
##cacheSolve function computes the inverse and cache the result
## by taking the result values of makeCacheMatrix function.
## It will first get and check whether the cached inverse matrix
##is the same as what needs to be computed if so, the cached inverse 
##matrix will be returned. Else this function will use solve() inbuilt 
##function to compute the inverse of the matrix.


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  #Get the invers Matrix
  Matinv <- x$getinv()
  

  if(!is.null(Matinv)) {
    message("This is cached matrix")
    return(Matinv)
  }
  
 ##Getting the matrix values
  getMat <- x$get()
  
  # Compute the inverse
  Matinv <- solve(getMat, ...)
  
  # Caching the inverse matrix value
  x$setinverse(Matinv)
  
  Matinv    
}
