---
title: "cachematrix"
author: "Jose Carlos Arriola"
date: "14 February 2016"
output: html_document
---

Lexical Scoping - My Solution
===============

Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). In this assignment a pair of functions are implemented that cache the inverse of a matrix: makeCacheMatrix and cacheSolve.

**makeCacheMatrix**

Creates a special "matrix", which is really a list containing a function to

1. set the value of the matrix
2. get the value of the matrix
3. set the value of the inverse matrix
4. get the value of the inverse matrix

```{r}
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
```

**cacheSolve**

Computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

```{r}
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("Getting cached data ...")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
```

Testing

```{r}
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
```
```
>    [,1] [,2]
[1,]    1    3
[2,]    2    4
```

```{r}
my_matrix$getinv()
```
```
> NULL
```

```{r}
cacheSolve(my_matrix)
```
```
>     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
```

```{r}
cacheSolve(my_matrix)
```
```
> Getting cached data ...
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
```

```{r}
my_matrix$getinv()
```
```
>     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
```
