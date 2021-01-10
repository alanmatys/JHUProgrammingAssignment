#Objective = Cache the inverse matrix


## special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  
  # NULL to start inverse value
  i <- NULL
  
  # Method: Set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  # Method: get the matrix
  get <- function() {
    m
  }
  
  # Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method: get the inverse of the matrix
  getInverse <- function() {
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  
  if( !is.null(m) ) {
    message("Obtaining data cache")
    return(m)
  }
  
  # get the matrix
  data <- x$get()
  
  # calculate the inverse w/ matrix multiply
  m <- solve(data) %*% data
  
  # set the inverse to the object
  x$setInverse(m)
  
  # return matrix
  m
}
}