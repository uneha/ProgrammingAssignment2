##Assignment for Coursera 
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix<-function(x=matrix()){  ## define the argument with default mode of "matrix"
  inver<-NULL                           ## initialize inver as NULL; will hold value of matrix inverse
  set<-function(y){
    x<<-y
    inver<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)        ## assigns value of inver in parent environment
    inver<<-inverse
  getinverse<-function()inver          ## gets the value of inver where called
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve<-function(x,...){
  inver<-x$getinverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data<-x$get()
  inver<-solve(data,...)
  x$setinverse(inver)
  inver
}


