makeCacheMatrix <- function(x = matrix()) { ## define matrix
  
  m <- NULL    ## initalization m =NULL
  
  set <- function(y) {   ## define function
    
    x <<- y  ## matrix from other environment
    
    m <<- NULL ##  reassign m as null
  }
  
  get <- function() x  ##  define a function for matrix argument
  
  setInv <- function(inverse) m <<- inverse ## assign value of inverse in other environment
  getInv <- function() m   #get the value of m
  
  list(set = set, get = get,  ## create a list for all values
       
       setInv = setInv,  getInv = getInv)
}

cacheSolve <- function(x, ...) {  ## got a inverse matrix from above function
  
  m <- x$getInv()  ## get values from parent function
  
  if(!is.null(m)) {  ## condition placed to get cached data
    
    message("getting cached data")
    
    return(m)
    
  }
  
  data <- x$get()       ## get value of matrix from parent function
  
  m <- solve(data, ...) ## inverse action done
  
  x$setInv(m) ## get values of the matrix
  
  m  ## return the value
  
}