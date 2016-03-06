## The functions will return a cached inverse of
# a matrix or will generate an inverse if not availible


## This function generates the cached list functions

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  #This will set the value of the matrix
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  #This will get the value of matrix
  get <- function() x
  #This sets the inverse
  setinv <- function(inv) i <<- inv
  #Gets the inverse
  getinv <- function() i
  #Generate list
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## This function checks for cached values and if not present
# will generate the value (inverse)

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  #check for cached inverse
  if(!is.null(i)){
    message("Pulling from the cached data.")
    return(i)
  }
  #get the result if possible
  data <- x$get()
  #If not, compute the inverse and set it in cache
  i <- solve(data)
  x$setinv(i)
  i
}
