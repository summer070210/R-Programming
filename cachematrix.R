## The functions can cache the inverse of a matrix

## Create a special matrix object to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ##initialize the inverse property
  m<-NULL
  
  #Set the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  ##Get the matrix
  get<-function() x
  
  ##Set the inverse of matrix
  setinverse<-function(inverse) m<<-inverse
  
  ##Get the inverse of matrix
  getinverse<-function() m
  
  ##Return the list of methods
  list(set=set,get=get,
       setmean=setmean,
       getmean=getmean)

}


## Compute the inverse of the matrix 
## The matrix was returned by the above makeCacheMatrix function
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  
  ##Return the inverse if already set
  if(!is.null(m)){
    message("geeting cached data")
    return (m)
  }
  
  data<-x$get()
  
  ##Compute the inverse using Solve function
  m<-solve(data)
  
  x$setinverse(m)
  m
}
