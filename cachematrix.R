## Matrix inversion is usually a costly computation and there is some 
## benefit in caching the inverse of a matrix instead of computing
## it repeatedly. The functions below cache the inverse of a matrix.



## This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## x: square invertible matrix
      ## returns: a list containing functions to
      ## 1.set the matrix
      ## 2.get the matrix
      ## 3.set the inverse 
      ## 4.get the inverse 
      
      inv=NULL
      
      set<-function(y){
            
            x<<-y
            inv<<-NULL
      }
      
      get<-function()x
      setinv=function(inverse) inv<<-inverse
      getinv=function() inv
      list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## This function computes the inverse of the"special" matrix returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix
## has not changed), then the cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse<-x$getinv()
      
      # if the inverse has already been calculated
      if(!is.null(inv)){
            # get it from the cache
            message("getting cached data")
            return(inverse)
      }
      # otherwise, calculate the inverse    
      data<-x$get() # get the value of the input matrix
      x$set() # run the set function on the input matrix to cache it
      inv<-solve(data,...) # compute the inverse of the input matrix
      x$setinv(inv) # cache the inverse
      inv # return the inverse
}
