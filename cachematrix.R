## Entering a matrix whose the inverse matrix is calculated, create a list that contains a function to 
## 1.set the matrix-2.get the matrix-3.set the inverse matrix-4.get the inverse matrix.

## "makeCacheMatrix" calculate the inverse matrix of a given matrix and store the output into a list that
## contains the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
        setmatrix=setmatrix,
        getmatrix=getmatrix)
}


## If the inverse matrix of the given matrix has already been calculated "cacheSolve" print the inverse matrix
## If not, the inverse matrix will be calculated and stored into a list. 

cacheSolve <- function(x=matrix(), ...) {
            m<-x$getmatrix()
            if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
