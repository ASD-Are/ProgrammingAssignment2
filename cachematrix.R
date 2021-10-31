## Computing the inverse of fuctions is costly, so caching it's inverse would save a great processing

## This function creates a list of functions 
#The first sets the value of the matrix
#The second gets the value of the matrix
#The third sets the inverse of the matrix
#The fourth gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()){
  inv = NULL
  
  set <- function(){
    x<<-y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <<- function(inverse) inv<<- inverse
  
  getinv <<- function() inv
  
  list(set= set, get = get, setinv = setinv, getinv = getinv)
  
}


#This function returns the inverse of a matrix but first it checks if the inverse of the matrix already exists or not in order not to do the same process again and again 
cacheSolve <- function(x, ...){
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
}



