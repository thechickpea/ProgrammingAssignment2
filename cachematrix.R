## Coursera Assignment #2
## This function calculates the inverse of a matrix, that it is a long
## calculation when we have a big matrix.
## With this two functions we can avoid the calculation of the 
## inverse matrix that we already calculate

## The first function 'makeCacheMatrix' creates a NULL special 'matrix' wich actually is a list
## that contains a function that: 

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
                inverse <- NULL ## set the value of the inverse as NULL
                set <- function(y) {
                        x <<- y
                        inverse <<- NULL
                }
                get <- function() x
                setmatrix <- function(solve) inverse <<- solve
                getmatrix <- function() inverse
                list(set = set, get = get,
                     setmatrix = setmatrix,
                     getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                inverse <- x$getmatrix()
                ## if the inverse has already been calculated we get a message saying that
                if(!is.null(inverse)) {
                        message("getting cached data")
                        ## This means that we are skipping calculations
                        return(inverse)
                }
                ## or if we haven't calculated the inverse of this matrix, it calculates it
                data <- x$get()
                inverse <- solve(data, ...)
                x$setmatrix(inverse)
                inverse
}

## Testing
x <- rbind(c(9, 1), c(2, -9))  #This is a 2x2 matrix
z <- makeCacheMatrix(x)  ## Using z because is already in use
z$get() 

## if we use the regular command solve (just to check our result)
solve(x)

## Using the formula:

cacheSolve(z)

## In the first iteration we won't see the message but when we re-run it
cacheSolve(z)

## we get the 'getting cached data' in the console
## That's it :)


