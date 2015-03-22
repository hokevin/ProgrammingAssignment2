## These two functions demonstrate how values can be cached to be used
## later by other functions.  

## the following function reads data in from a matrix
## gets its inverse and stores it in a special vector that 
## will allow other functions to access it.  If not done correctly that
## inverse will only be available locally to this function and will disappear
## after the function is  closed.
## this functions only argument is the name of the matrix that we need to 
## obtain the inverse for

makeCacheMatrix <- function(x = matrix()) {           ## reads value into a matrix

  
m <- NULL                  ## need to initialize or reset m - 
                           ## which will contain the inverse matrix - must do
                           ## or function will fail the first time through
set <- function (y){       ## set initalizes the matrix that is passed in
  
  x<<- y
  m<<- NULL
}
                            ## these next steps create the functions that
                            ## will be used in cacheSolve
get <- function()x
setmatrix <- function (solve) m<<-solve
getmatrix <- function () m

## the following step creates a list which contains the list of
## named functions e.g. set, get, setmatrix and getmatrix created
## in this function

list (set=set, get = get, setmatrix = setmatrix,
      getmatrix = getmatrix)

}


## This function finds the inverse of the matrix x.
## before computing the inverse it first checks in getmatrix
## created in the makeCacheMatrix for a cached result.


cacheSolve <- function(x, ...) {   ## reads in passed in matrix into x
                                   ## this next section checks to make see if
                                   ## this function has run before by checking
                                   ## the value of m  - if it is not null then it 
                                   ## uses the cached value if it empty then  
                                   ## it computess the inverse of target matrix
                                  
  m<-x$getmatrix()
  if (!is.null(m)) {
    message ("getting cached data")     ## if available it uses the cached solution
    return (m)
    
  }
  
  matrix <-x$get()                      ## if not already available it computes inverse
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
  
}
## Instructions on running these two functions
## first we need to construct the matrix that will be fed into makeCacheMatrix
## do this my creating a vector containing the data you will put into 
## the matrix 
## a<- 1:4   
## then create the matrix
## x <- matrix (a, 2,2)  (creates a 2X2 matrix)
## call the first function as follows
## matrix <- makeCacheMatrix (x)    the returned list is saved in "matrix"
## call the second function as follows
## cacheSolve (matrix)
## this returns the inverse of matrix
## you can check this by comparing the output of the function to the value
## of solve(matrix)