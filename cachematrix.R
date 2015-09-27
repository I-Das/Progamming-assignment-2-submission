## The two large functions below are used to create an object that stores a matrix and 
## then cache's its inverse.

## The first function, 'makeCacheMatrix' creates a  matrix that is a list
##containing separate function to: 1) set the value of the matrix. 2) get the value of the
##matrix.3) set the value of the inverse 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix(data, ...)) {
          i <- NULL # assigns value of inverse to null to start with
          
          set <- function (y){
                  x <<- y
                  i <<- NULL ## changes the stored matrix into the input 'y'
          }
          
        get <- function() x
        setinverse <- function(solve) i <<- solve ## stores the value of inverse 
        getinverse <- function () i 
        list(set = set, get = get,  ## stores all the functions within 'makeCacheMatrix'
             getinverse = getinverse, 
             setinverse = setinverse)

}


## The following function calculates the inverse of the matrix created by the above
##function. It only computes the inverse if it finds that the inverse for the matrix
## is not already stored in the cache. If the inverse is already stored in the cache
## then it skips the computation and 'gets' it from there.

cacheSolve <- function(x, ...) {
       i <- x$get()
       if(!is.null(i)){ ## checks to see if the inverted matrix is stored in cache
                 message("getting cached data")
                 return(i)
       }
       answer <- x$get()
       i <- solve(answer, ...) ## computes inverse of created matrix
       x$setinverse(i) ## sets the value of the inverted matrix in cache
       i
}
 