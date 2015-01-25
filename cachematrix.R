## Put comments here that give an overall description of what your
## functions do

## Using this function, we make the special matrix which can be stored and called by functions described in this function

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  # Inside this makematrix function, you are defining list of functions set, get, setinverse, getinverse
  
  set <- function(y) {
    
    x <<- y
    
    i <<- NULL
    
  }
  
  # The above function set is used to set the value of matrix
  
  get <- function() x
  
  # The above function get is used to get the value of the matrix that we set
  
  setinverse <- function(inverse) i <<- inverse
  
  # The above function setinverse is used to set the inverse value of the matrix
  
  getinverse <- function() i
  
  # The above function getinverse is used to get the inverse value of the matrix 
  
  list(set = set, get = get,
       
       setinverse = setinverse,
       
       getinverse = getinverse)

}


## cacheSolve is for getting inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Here, we are using getinverse function to get inverse of the matrix x
  i <- x$getinverse()
  
  # If it is already calculated, then it will be returned with a message "getting cached data"  
  if(!is.null(i)) {
    
    message("getting cached data")
    
    return(i)
    
  }
  
  # If it's not calculated, we get the data i.e, value of matrix using get() function from makematrix function
  data <- x$get()
  
  # Then, we get the inverse of matrix using solve function 
  i <- solve(data)
  
  # Then, we set/store the inverse of matrix using setinverse function from makematrix function
  x$setinverse(i)
  
  # print the inverse of matrix
  i
}
