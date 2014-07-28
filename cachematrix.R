#----------------------------------------------------------------------------
#NOTE FOR PEER-REVIEWERS: if you are reading this you are looking at a different
# commit with respect to that submitted. This an enhanced versions that fixes some 
# design inefficiences present in the suggested starting script. 
# As a matter of fact, this way the cacheSolve function is no longer needed and all is encapsulated
# within the makeCacheMatrix.
# You should evaluate the other one but any comment or suggestion on this version is welcome :)
#----------------------------------------------------------------------------

##The following functions allow to chache the inverse of a matrix.
##This, allows to instantly return the inverse when needed and avoids a computation overhead

##This function builds a matrix "class" that allows to store a cached copy of the inverse
makeCacheMatrix <- function(savedMatrix = matrix()) 
{
    cachedInv <- NULL
    #function that (re)sets the matrix
    set <- function(y) 
    {
        #I replace the saved matrix only if the new one is different from the previous one
        if(!matequal(savedMatrix,y))
        {
            savedMatrix <<- y
            cachedInv <<- NULL
        }
    }
    #function that returns the saved matrix
    get <- function() return(savedMatrix)
    
    #function that sets the inverse. 
    #If called without args the inverse is computed from scratch
    setInv <- function(inverse,...) 
    {
        if(missing(inverse))
        {
            cachedInv <<- solve(savedMatrix, ...)
        }
        else 
        {
            cachedInv <<- inverse
        }
    }
    #function that returns the inverse either cached or computed from scratch
    getInv <- function(...) 
    {
        if(!is.null(cachedInv))
        {
            message("getting cached data")
            return(cachedInv)
        }
        else
        {
            message("computing the inverse")
            setInv()
            return(cachedInv)
        }        
    }
   
    # auxiliary functions: 
    # this function compare two matrices and check if they are identical
    matequal <- function(x, y)
    {
        return(is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y))
    }     
    
    # aggregate and return visible functions
    return(list(set = set, get = get,
                setInv = setInv,
                getInv = getInv))   
}

## This function returns a matrix that is the inverse of 'x'
## If already computed before it returns the cached version
## with the modification made to the makeCacheMatrix class this function is useless...but kept for backward compatibility
cacheSolve <- function(x, ...) 
{ 
    return(x$getInv())
}
