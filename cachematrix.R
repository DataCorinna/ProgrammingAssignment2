## This file contains two R functions that were written for
## the second programming assignment of the Coursera course
## 'R programming'. Their purpose is to be an example for
## caching calculated values to avoid having to calculate
## the same values several times (e.g. in a loop).

## The function 'makeCacheMatrix' caches the inverse of
## a matrix that is put into the function.

## The function 'cacheSolve' returns the inverse of a 
## matrix that is put into the function - only that it
## checks first, if the inverse of this matrix has
## already been calculated and cached by a previous
## call of 'makeCacheMatrix'. In this case it skips
## the calculation and retrieves the inverse from the
## cache.

## The function 'makeCacheMatrix' returns a list with four
## list items to the calling environment. The item names
## are 'set', 'get', 'setinverse' and 'getinverse.
## The contents of each list item is a local function
## with the same name as the list item. These four local
## functions do the actual caching of the matrix an its
## inverse.

makeCacheMatrix<-function(x=matrix()){
  
  ## initialize cache of inverse in local environment: 
  m<-NULL
  
  ##define local function 'set' (=set variable in calling environment):
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  ##define local function 'get' (=retrieve matrix to calling environment)
  get<-function() x
  
  ##define local function 'setinverse' (=set inverse of matrix)
  setinverse<-function(solve) m<-solve
  
  ##define local function 'getinverse' (=retrieve inverse to calling env.)
  getinverse<-function() m

  ##return a four-item-list to the calling environment, containing
  ##the four locally defined functions above, calculated for the
  ##matrix that has been put into 'makeCacheMatrix' when the
  # function was called:
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse
  )
}


## The following function "cacheSolve" needs as input a
## four-item-list that has been created using the function
## 'makeCachemean'. If the inverse has already been
## calculated and cached, it retrieves the inverse from
## the cache, otherwise it calcutes the inverse. It returns
## the inverse to the calling environment.
## To see the difference, in the console (in debug mode for
## both functions try either ...
## 1st: z<-matrix(c(5,8,2,5), nrow = 2, ncol = 2)
## 2nd: a<-makeCacheMatrix(z)
## 3rd: cacheSolve(a)
## or try ...
## 1st: z<-matrix(c(5,8,2,5), nrow = 2, ncol = 2)
## 2nd: cacheSolve(makeCacheMatrix(z))

cacheSolve<-function(x,...){
  
  ##find out, if the inverse has already been created:
  m<-x$getinverse()
  
  ## if it has, retrieve it from the cache and return it to
  ## the calling environment:
  if(!is.null(m)){
    message("getting cached inverse")
    return(m)
  }
  
  ##if the inverse has not yet been cached, then calculate
  ##it, cache it an return it to the calling environment:
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m

}
