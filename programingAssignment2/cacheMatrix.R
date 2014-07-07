## Put comments here that give an overall description of what your
## functions do

##create a function which starts with a null matrix argument

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL   ## initialize the value of the matrix inverse to NULL
  
  ##delcare another function set where the value will be cached in
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## gets the value of the matrix
  get <- function() x
  
  ## sets the value of inverse of matrix
  setinverse <- function(inverse) i <<- inverse
  
  ## gets the value of inverse of matrix
  getinverse <- function() i
  
  ##return the list of the fuctions
  list(set = set , get = get, 
       setinverse = setinverse,
       getinverse = getinverse )
  
  
}



## this function This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will return cached matrix

cacheSolve <- function(x , ...) {
  i <- x$getinverse()
  
  ##check whether inverse is already calculated or not 
  if(!is.null(i)) {
    message("Getting Cached Data");
    return(i)
  }
  
  data <- x$get()
  
  i <- solve(data, ...)
  
  x$setinverse(i)
  
  i
  
}