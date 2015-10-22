##makeCacheMatrix creates and returns a list of functions which are
## used by cacheSolve to retrieve the inverted matrix or set 
## the inverted matrix in cache
makeCacheMatrix <- function(x = matrix()) {
# store cache value and initialize to null
  mycache <- NULL
  set <- function(y) 
  {
    x <<- y
    mycache <<- NULL
  }
  #get the value of the matrix
  get <- function()x
  #invert matrix and store in cache
  setinverse <- function(inverse) mycache <<- inverse
  getinverse <- function()mycache
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)


}


## Write a short comment describing this function
##cacheSolve calculates inverse of matrix from function makeCacheMatrix.
## if the inverted matrix does not exit, it will get created.
## otherwise it is stored in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 mycache <- x$getinverse()
  if(!is.null(mycache))
  {
    message("getting cache data.")
    return(mycache)
    
  }
  data <- x$get()
  
  tryCatch(
    {
      mycache <- solve(data,...)
      
    },
    error = function(e)
    {
      message("Error exception")
      message(e)
      return(NA)
      
    },
    warning = function(e)
    {
      message("Warning error")
      message(e)
      return(NA)
      
    },
    finally = {
      x$setinverse(mycache)
      return(mycache)
      
    }
  )
  return(mycache)
  	
		
		
		
}
