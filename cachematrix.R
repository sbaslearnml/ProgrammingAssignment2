## Cretae a special matrix (makeCacheMatrix - actually a list that is holding a matrix) and some functions as mentioned below
## set - initialize the matrix
## get - return the matrix
## setInverse - initialize the inversedMatrix 
## getInverse - return the inversedMatrix


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


## Function cacheSolve operates on the special matrix makeCacheMatrix, to look for cached value (inverse matrix)
## ad return it. In case, if it could not find the cached value, it calculates and caches the value

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
