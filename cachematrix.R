## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  # the set functions take an argument and sets that as the matrix that can be cached
  set<- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  # retrieves the matrix
  setinverse<- function(inverse) m<<-inverse
  # gets the inverse function as an argument and sets that as the inverse variable for the list
  getinverse<- function() m
  #retrieves the inverse function
  list(set= set, get=get,setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinverse(m)
  m
}

x<- matrix(data= c(0,1,3,4,5,5,6,6,7), nrow=3, ncol = 3)
y<-makeCacheMatrix(x)
cacheSolve(y)
