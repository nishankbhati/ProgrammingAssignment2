## -------------------AUTHOR:-Nishank Bhati---------------------
#The LNM Institute if Information Technology, CSE major (Pre-final year)


#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a 
#matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will
# not discuss here).
# Your assignment is to write a pair of functions that cache the inverse of a matrix.

## The first function,, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to:-
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of inverse
## 4.Get the value of inverse 

makeCacheMatrix <- function(x=matrix()){
  temp<- NULL
  set <- function(y){  #set the value of matrix
    x<<-y
    temp<<- NULL
  }
  
  get<-function() x #Get the value of matrix
  setinverse <- function(inverse) temp<<-inverse   # set the value of inverse
  getinverse <- function()temp                     # Get the value of inverse 
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
  
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    temp <- x$getinverse()
    
    if(!is.null(temp)) {
    message("getting cached data.")
    return(temp)
    }
    
   data <- x$get()
   temp <- solve(data)
   x$setinverse(temp)
   temp
  
  ## Return a matrix "temp" that is the inverse of "x"
}
