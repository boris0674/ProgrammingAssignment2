## in order to complete my assignment I used the same template 
## that was provided by Coursera
## in the example in order to cache the mean of a vector
## first i wrote the makeCacheMatrix function in order to:
## set the value of the matrix
## get the value of the matrix
## set the value of inverse
## get the value of the inverse
## in this case the arguement used as input is not a vector anymore
## it is an invertible matrix
## initially the inverse matrix is non-defined (NULL)


makeCacheMatrix <- function(x = matrix()) {

inversa <- NULL

set <- function(y) {

x <<- y

inversa <<- NULL

}
get <- function() x

setinverse <- function(inverse) inversa <<- inverse


getinverse <- function() inversa

list(set=set, get=get, setinverse=setinverse,

getinverse=getinverse)

}

## then I wrote the the cacheSolve function in order to:
## return the inverse of any invertible matrix I might submit
## when provided with a new matrix it checks 
## if the inverse has already been cached previously
## as it cannot be found, it is then  calculated with the  setinverse function
## only to be retrieved later from the cache 
## if we rerun cacheSolve with the same input matrix


cacheSolve <- function(x, ...) {

inversa <- x$getinverse()

if(!is.null(inversa)) {

message("getting cached data.")

return(inversa)

}
data <- x$get()

inversa <- solve(data)

x$setinverse(inversa)

inversa

}

## let's try out an example
## i first convert into a matrix the first 5 lines of a dataframe called artframe
## this data frame features the quarterly yields of some specific art markets
## in our case contemporary art, post WW2 art (dopoguerra in italian)
## paintings prints and sculptures

artmatrix <- as.matrix(artframe[1:5,1:5])

## here's the printed result

print(artmatrix)


  contemporary dopoguerra paintings     prints sculptures
1    3.6199095 -3.1620553  0.781250  3.2258065  2.4590164
2    5.7416268 -4.5283019  0.000000  0.8130081  3.3898305
3    0.9661836  0.0000000 -1.538462 -1.6000000  0.8547009
4   -1.8957346  1.1450382 -1.515152 -3.1007752 -2.5000000
5    7.1065990 -0.7575758  0.000000 -0.7692308 -1.6393443


## let's check out if this matrix is invertible
## in order to do that i write a logical object called invertible
## invertible checks if the absolute value of the determinant of our matrix
## is indeed NOT equal to zero


invertible <- abs(det(artmatrix)) > 0

> invertible
[1] TRUE

## so we can use artmatrix in our experiment


## I create an object using artmatrix as the input data into makeCacheMatrix

try <- makeCacheMatrix(artmatrix)

## I retrieve the data of our matrix 

try$get()

 try$get()
  contemporary dopoguerra paintings     prints sculptures
1    3.6199095 -3.1620553  0.781250  3.2258065  2.4590164
2    5.7416268 -4.5283019  0.000000  0.8130081  3.3898305
3    0.9661836  0.0000000 -1.538462 -1.6000000  0.8547009
4   -1.8957346  1.1450382 -1.515152 -3.1007752 -2.5000000
5    7.1065990 -0.7575758  0.000000 -0.7692308 -1.6393443


## and as we can see if I initially try to see if there's a cached inverse
## I cannot find it, the getinverse function returns 
## the NULL value we set up as default

try$getinverse()

> try$getinverse()
NULL


## we then use our try object as an input into cachSolve
## which calculates the inverse of artmatrix and set it into makeCacheMatrix

cacheSolve(try)

> cacheSolve(try)
                       1           2          3          4           5
contemporary -0.02293552 -0.03803481  0.1110326 -0.1245669  0.13480182
dopoguerra   -0.15799994 -0.23848735  0.3461669 -0.4329613  0.11060289
paintings    -0.85580830  0.48729896 -0.6682099 -0.4227861  0.02028860
prints        0.63561308 -0.41636468  0.2048713  0.1197148  0.01671025
sculptures   -0.32466014  0.14069932  0.2252262 -0.3960941 -0.08458359
> 

## let's try another run

cacheSolve(try)
getting cached data.
                       1           2          3          4           5
contemporary -0.02293552 -0.03803481  0.1110326 -0.1245669  0.13480182
dopoguerra   -0.15799994 -0.23848735  0.3461669 -0.4329613  0.11060289
paintings    -0.85580830  0.48729896 -0.6682099 -0.4227861  0.02028860
prints        0.63561308 -0.41636468  0.2048713  0.1197148  0.01671025
sculptures   -0.32466014  0.14069932  0.2252262 -0.3960941 -0.08458359
>

## as you can see from the "getting the cached data" 
## now the values of artmatrix inverse were cached 
## This was not an easy task to complete !