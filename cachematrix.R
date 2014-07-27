##The following functions allow to chache the inverse of a matrix.
##This, allows to instantly return the inverse when needed and avoids a computation overhead

##This function builds a matrix "class" that allows to store a cached copy of the inverse
makeCacheMatrix <- function(savedMatrix = matrix()) 
{
    cachedInv <- NULL
    set <- function(y) 
    {
        #I replace the saved matrix only if the new one is different from the previous one
        if(!matequal(savedMatrix,y))
        {
            savedMatrix <<- y
            cachedInv <<- NULL
        }
    }
    get <- function() savedMatrix
    setInv <- function(inverse) cachedInv <<- inverse
    getInv <- function() cachedInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)    
}


## This function returns a matrix that is the inverse of 'x'
## If already computed before it returns the cached version
cacheSolve <- function(x, ...) 
{
    cachedInv <- x$getInv()
    if(!is.null(cachedInv)) 
    {
        message("getting cached data")
        return(cachedInv)
    }
    savedMatrix <- x$get()
    cachedInv <- solve(savedMatrix, ...)
    x$setInv(cachedInv)
    cachedInv
}

# this function compare two matrices and check if they are identical
matequal <- function(x, y)
{
    is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
} 
