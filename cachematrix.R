#This function creates a special matrix
 

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



#This function takes the special matrix created by makeCacheMatrix and returns the inverse from cache if exists
#If does not exists - it will calculate and cache it.


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
 
