## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Caches the inverse matrix
# returns and store the cache
makeCacheMatrix <- function(x = matrix()) {
  
  cachedMat<-NULL
  
  set<-function(y)
  {
    x<<-y
    cachedMat<<-NULL
  }
  
  get<-function() x
  setcachematrix<-function(mat) cachedMat<<-mat
  getchachematrix<-function() cachedMat
  
  # return
  list(set = set, get = get, setcachematrix = setcachematrix,getchachematrix = getchachematrix )
}


## Write a short comment describing this function
# Calculates the inverse matrix
# if already cached, then returns the cached matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  imat<-x$getchachematrix
  if(!is.null(imat))
  {
    message('getting cached data')
    return(imat)
  }
  data<-x$get
  imat<-solve(data,...)
  x$setcachematrix(imat)
  imat
}
