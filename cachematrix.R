## This method is used to find the inverse of the matrix
## and cache it in memory for easier access

## This is used to cache the matrix and the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(solve) inv<<-solve
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## this is used to retrieve the cached matrix and solve it to produce the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}