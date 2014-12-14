## 可以缓存逆矩阵的两个函数功能


## makeCacheMatrix函数用于创建可缓存逆矩阵的特殊“矩阵”对象。
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinver <- function(inverse) i <<- inverse
  getinver <- function() i
    ##创建List列表，分别储存四个函数set,get,setinver,getinver。
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)

}


## 此函数用于计算makeCacheMatrix返回的特殊“矩阵”的逆矩阵
cacheSolve <- function(x, ...) {
        ## 返回传入矩阵的逆矩阵
  i <- x$getinver()
        ##如果已经计算逆矩阵，检索缓存中的逆矩阵应存在。
  if(!is.null(i)) {
    message("getting cached data")   ##输出提示并返回缓存中得逆矩阵
    return(i)
  }
        ##否则重新计算逆矩阵并返回。
  data <- x$get()
  i <- solve(data)
  x$setinver(i)
  i
}
