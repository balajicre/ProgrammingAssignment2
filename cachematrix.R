## This function computes the inverse and uses the value from cache if it exists
## if not, computes the inveser using the solve function, always assume input
## is invertible

## Function that caches a matrix

makeCacheMatrix <- function(x=matrix()) {
      inverseMatrix<-NULL
	setMatrix<-function(y){
		x<<-y
		inverseMatrix<<-NULL
	}

	getMatrix<-function() x

      setInverseMatrix<-function(inverse){
            inverseMatrix<<-inverse
	}
      getInverseMatrix<-function() inverseMatrix

      list(setMatrix=setMatrix,getMatrix=getMatrix,setInverseMatrix=setInverseMatrix,getInverseMatrix=getInverseMatrix)

}


## Function that takes in a makeCacheMatrix function, and another matrix
## Checks if the matrix is identical with the cache matrix and computes
## the inverse only if it is not cached.

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix<-x$getInverseMatrix()
        if(is.null(inverseMatrix)){
            print("calculating inverse")
           	inverse<-solve(x$getMatrix())
            print("setting inverse matrix")
            x$setInverseMatrix(inverse)
            return(inverse)           
	  }else{
 		print("get inverse from cache")
		inverse<-x$getInverseMatrix()
            return(inverse)
        }
}
