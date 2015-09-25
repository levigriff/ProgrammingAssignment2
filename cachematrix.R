##Below are two functions that are used to create a special object that stores a matrix and caches its inverse
##makeCacheMatrix function creates a special "matrix" object that can cache its inverse 
##and should be assigned to a variable like x <- makeCacheMatrix

##This function stores a matrix and its inversion into a 'cache' variable
makeCacheMatrix <- function(x = matrix()) { 
        
        ## It returns: a list containing four functions used to
        ##              1. set the matrix (assign matrix to a variable)
        ##              2. get the matrix (recall the existing matrix)
        ##              3. set the inverse (assign inverted matrix to the variable)
        ##              4. get the inverse (recall the stored inverted matrix)
        ##         this list is used as the input to cacheSolve()
        
        m <-NULL##set inverse buffer to NULL on creation of the matrix variable
        set <-function(y){ 
                
                ##Use the <<- to cause a search to be made through parent environments 
                ##for an existing definition of the variable being assigned 
                x<<-y
                m<<-NULL ##remove any record of previously inverted matrix
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## This function examines the cached matrix and computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m<-x$getmatrix() ##check to see if there is a cached inverted matrix
        if(!is.null(m)){ ##if there is...
                message("getting cached data") ##display this message
                return(m) ## and return the inverted matrix
        }
        matrix <- x$get() ##get the cached matrix
        m <- solve(matrix, ...) ##invert the matrix
        x$setmatrix(m) ##store the inverted matrix to cache
        m ##return the inverted matrix
}
