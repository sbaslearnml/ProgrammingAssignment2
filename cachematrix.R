## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inversedMatrix <- NULL

# set/initialize the matrix
set <- function(y)
{
    x<<- y
    inversedMatrix <<- NULL
}

#get/return the matrix
get <- function() x

#calculate the matrix inversion (using solve fn) and set/initialize it
setInverse <- function (y)
{
    inversedMatrix <<- y
}
# get/return the inversed matrix
getInverse <- function()
{
    inversedMatrix
}

# list that makes the special matrix
list (get = get,
      set = set,
      getInverse = getInverse,
      setInverse = setInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # call getinverse function, if cache is availlable then
    # will retun the expected value, if not will retun NULL
    inversedM <- x$getInverse()
    
    #if value is found, the return the value else call the setinverse function and return
    if(!is.null(inversedM))
    {
        message("returning the cached value - inversed matrix")
        return(inversedM)
    }
    else
    {
        rawmatrix <- x$get()
        inversedM <- solve(rawmatrix)
        x$setInverse(inversedM)
        inversedM
    }
    
}
