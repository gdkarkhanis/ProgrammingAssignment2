## Girish Karkhanis - R Programming - Assignment 2
## Chenge from example mean to solve functions

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## Initializing the inverse to null
        set <- function(y) {
                x <<- y  ## Assigning new matrix to x using set
                i <<- NULL
        }
        get <- function() x
        ##Storing the inverse in i after it is calculated
        setsolve <- function(solve) i <<- solve 
        
        ##Getting the value from cache if available
        getsolve <- function() i 
        
        ##Store the values in a list and return the list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Calculate inverse of a matrix.
cacheSolve <- function(x, ...) {
        i <- x$getsolve() #Check if matrix x is cached
        if(!is.null(i)) {
                message("getting cached data") #If the matrix x is cached than get the cached value
                return(i)
        }
        data <- x$get() #If matrix is not cached we get it and caclculate the inverse
        i <- solve(data, ...) 
        x$setsolve(i) #Cache the Inverse of the matrix that was created above
        i
}

## Trial
x <- makeCacheMatrix(matrix(1:4,2))
z<-x$get() ##Get the matrix created
z

cacheSolve(x) ## Get the inverse
cacheSolve(x) ## Check if cached inverse is returned

## If code is correct y %*%y should be id-matrix
y<-x$getsolve()
y %*% z 
