
## makeCacheMatrix
## Description: Accepts matrix and prepares the Object holding the 
##       functionality of child functions to retrive save and retrive the 
##       assigned Matrix and Inverse Matrix ...or to say anything assigned to save
##    No Validation on Validity of Inverse or Calculation of determinent etc.
makeCacheMatrix <- function (x = matrix()) {
  
  print( paste0("makeCacheMatrix:Rows:", nrow(x), "Columns:",ncol(x)))
  
  ## resultant Matrix object holder
  im<-NULL
  
  ## Inner function - Will be used to Assign New Matrix
  set <- function(y) {
    x<<-y
    im<<-NULL
    
  }
  
  ## Inner Function used to retrieve Matrix on calling get
  get <- function() x
  
  setInv <- function(inv) im<<-inv
  getInv <- function() im
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}



### cacheSolve:
### Description:  takes the child functions that need to run and performs 
###   actual get and set operations on matrix and Inverse matrix uses 
###    Inner child functions
###     which are defined in object returned by makeCacheMatrix(matrix) 
###   In Process it even deduces the Inverse of the Original Matrix.
cacheSolve <- function(z, ...) {
  
  # retrieving the Inverse from send Object
  im2 <- z$getInv()
  
  if(!is.null(im2)) {
    message("Getting cached Inverse Matrix")
    return (im2)
    
  }
  
  ## Found No Inverse Matrix...fallback to getting Original Matrix
  data <- z$get()
  
  ## Validation  to Check the Square of Matrix as we are doing Inverse
  if(nrow(data) != ncol(data)) {
    stop("Matrix is not Square Matrix and Cannot be Inversed!.");
    return(data)
  }
    
  ## Performing Inverse. Error would be raised if Inverse is not found
  im2 <- solve(data, ...)
  
  ## Storing into Environment scope
  z$setInv(im2)
  
  ## Printing the Inverse Matrix
  im2
  
  #print( paste0("Rows in Original Matrix:", nrow(data)))
    
}

##TEST
## Cross-check to see the Inverse Validation
## t1<-matrix(rnorm(10),2,2)
###  b1<-makeCacheMatrix(t1)
###  b2<-cacheSolve(b1)
  ## check the Inverse Validation
  ##  identical(b2$getInv(),solve(t1) )
###  b3<-cacheSolve(b1)
####  Getting cached Inverse Matrix

