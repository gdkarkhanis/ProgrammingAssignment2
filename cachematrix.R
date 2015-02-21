##The below functions are basically to test the caching concept in R
##First we create a function that accepts a matrix and stores it in an environment.
##Next we create a function that calculates the inverse of a matrix and stores it in the same env
##So everytime we try to create an Inverse of a matrix which exists in cache, it feteches
##the result from cache instead of calculating it again, thus saving time!


## This function accepts a mtrix as input and returns a list of 4 items: set, get, setsolve, getsolve

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

## This function calculates the inverse of a matrix. If cached value of inverse is available it gets
## it from there else it calculates
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

##Test Cases
x <- makeCacheMatrix(matrix(1:4,2))
z<-x$get() ##Get the matrix created
z

cacheSolve(x) ## Get the inverse
cacheSolve(x) ## Check if cached inverse is returned

#Test if inverse is correct
y<-x$getsolve()
y %*% z ##Should be identity matrix