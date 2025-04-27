# This function will  store the cached inverse
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  # Set the matrix
  set <- function(y){
    x <<- y
    # Reset inverse when matrix changes
    m <<- NULL  
    
  }
  # Get the matrix
  get <- function()x
  # Set the inverse function
  setinverse <- function(inverse) m <<- inverse
  # Get the inverse 
  getinverse <- function() m
  # Return a list of functions
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}
# Check if inverse already cached
cacheSolve <- function(x, ...){
  # Create a matrix 
  my_matrix <- matrix(c(1,2,3,4), nrow =2, ncol=2)
  # Create the special matrix object 
  x<- makeCacheMatrix(my_matrix)
  # Run cacheSolve again to see the "getting cached data" message
  cacheSolve(x)
  
  
  # check if inverse is already cached
  m <-x$getinverse()
  #If cached, return it
  if(!is.null(m)){
    message("getting the cached data")
    return(m)
  }
  # Calculate inverse
  data <- x$get()
  # return matrix that is an inverse
  m <- solve(data)
  #Store and Cache the inverse
  x$setinverse(m)
  # return the new inverse
  m
  
}


