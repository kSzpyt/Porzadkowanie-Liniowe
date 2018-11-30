library(TSdist)
library(dplyr)

#dataframe- ramka danych(numeric) z nazwanymi wierszami oraz kolumnami
#x - factor zawierająca charakter zmiennych
# 1 - stymulatna
# 2 - destymulanta
# 3 - nominanta
foo <- function(data, x = c(1, 2, 3), method = c("hellwig", "mss"))
{
  if(!is.data.frame(data))
  {
    stop("data is not a data frame")
  }
  if(!is.factor(x))
  {
    stop("x is not a factor")
  }
  if(dim(data)[2] != length(x))
  {
    stop("wrong length of x (should be the same as number of variables in data frame")
  }
  
  #zamiana nominanty na stymulante
  n2s <- function(v)
  {
    aaa <- v
    l <- length(aaa)
    m <- median(aaa)
    for (x in 1:l) 
    {
      if(aaa[x] == m)
      {
        aaa[x] <- 1
      }
      if(aaa[x] < m)
      {
        aaa[x] <- (-1)/(aaa[x] - m - 1)
      }
      if(aaa[x] > m)
      {
        aaa[x] <- 1/(aaa[x] - m + 1)
      }
    }
    return(aaa)
  }
  
  #standaryzacja
  data <- scale(data)
  data <- as.data.frame(data)
  l <- dim(data)[2]
  r <- dim(data)[1]
  
  if(method == "hellwig")
  {
    patt <- NA
    #zamiana na stymulanty
    for (a in 1:l) 
    {
      if(x[a] == 2)
      {
        data[, a] <- -data[, a]
      }
      if(x[a] == 3)
      {
        data[, a] <- n2s(data[, a])
      }
      patt[a] <- max(data[, a])
    }
    data2 <- data
    data2 <- rbind(data2, "pattern" = patt)
    
    #odległosci euklidesowe
    ed <- sapply(1:r, function(x)
    {
      EuclideanDistance(as.numeric(data2[x,]), as.numeric(data2[r+1, ]))
    })
    
    data2 <- cbind(data2, "ed" = c(ed, NA))
    d0 <- mean(ed) + 2*sd(ed)
    si <- sapply(1:r, function(x)
    {
      1-(ed[x]/d0)
    })
    
    data2 <- cbind(data2, "Si" = c(si, NA))
    
    
    
    return(data2)
  }
  
  
  
  
}
