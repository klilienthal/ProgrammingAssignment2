## These functions cache the time-consuming operation of
## computing the inverse of a matrix.

## This function creates a special matrix object,sets the value of the matrix,
## the value of its inverse and caches it.

makeCacheMatrix <- function(x = matrix()) {
  v<- NULL
  set<-function(y){
    x<<-y
    v<<-NULL
  }
  get<-function()x
  setinv<-function(solve) v<<-solve
  getinv<-function() v
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## This function computes the inverse of the special matrix created
## by the first function. First it checks to see if the inverse has
## already been computed. If so, it retrieves it from the cache and
## returns the inverse matrix.

cacheSolve <- function(x, ...) {
  v<-x$getinv()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data<-x$get()
  v<-solve(data,...)
  x$setinv(v)
  v
}
 
       
