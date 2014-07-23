## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) m <<- solve
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse= setinverse,
                 getinverse = getinverse )
    }



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
           m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data");
                    return(m)
            }
            
            message ("Caching Data");
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
 
}


## Example ##################
##  g<-matrix(c(4,3,3,2),2)
## g
##      [,1] [,2]
## [1,]    4    3
## [2,]    3    2
## a<-makeCacheMatrix(g)
## cacheSolve(a)
##Caching Data
##     [,1] [,2]
##[1,]   -2    3
##[2,]    3   -4
## cacheSolve(a)
##getting cached data
##     [,1] [,2]
##[1,]   -2    3
##[2,]    3   -4
 
