## I have created two functions here, one which will create a matrix, stored as a list, and the other function will solve 
## the inverse of a matrix that is passed to it, or it will retrieve the inverse if it has already been calculated

## this function creates a matrix and stores it as a list. the list can store the matrix as well as the inverse for the matrix
## the get function retrieves the matrix, the getinverse function retrieves the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL #sets the inverse to be null if it has not previously been calculated
        }
        get <- function() x #retrieves the matrix from the list
        setinverse <- function(inverse) m <<- inverse #sets the inverse to be stored as part of the list
        getinverse <- function() m #retrieves the inverse from the list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) #creates the ability to retrieve these items from the list
}


## determines if the inverse already exists, if so, it will retrieve it, if not, calculates the inverse and stores it in
## the list

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) { #if there is already a value stored as the inverse (i.e. "m") then retrieve the value
                message("getting cached data")
                return(m) #shows the inverse previously calculated
        }
        data <- x$get() #if the inverse hasn't already been calculated retrieve items in x
        m <- solve(data, ...) #calculate the inverse of the matrix
        x$setinverse(m) #sets the inverse as the newly calculated value in the list
        m #spits out the inverse, just calculated
}