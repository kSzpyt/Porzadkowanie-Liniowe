library(TSdist)
library(dplyr)

#dataframe- ramka danych(numeric) z nazwanymi wierszami oraz kolumnami
#x - factor zawierająca charakter zmiennych
# 1 - stymulatna
# 2 - destymulanta
# 3 - nominanta
foo <- function(data, x = c(1, 2, 3), nom = NA, method = c("hellwig", "mss"))
{
  if(!is.data.frame(data))
  {
    stop("data is not a data frame")
  }
  # if(!is.factor(x))
  # {
  #   stop("x is not a factor")
  # }
  if(dim(data)[2] != length(x))
  {
    stop("wrong length of x (should be the same as number of variables in data frame")
  }
  #wyjątek gdy ilosć wartosci nominant jest rózna od nimonant
  
  #zamiana nominanty na stymulante
  nom2 <- nom
  n2s <- function(v, nom2)
  {
    aaa <- v
    l <- length(aaa)
    m <- nom2
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
  data_st <- scale(data)
  data_st <- as.data.frame(data_st)
  l <- dim(data)[2]
  r <- dim(data)[1]
  
  {
  #do zmainy data_st też
  # if(method == "hellwig")
  # {
  #   patt <- NA
  #   #zamiana na stymulanty
  #   for (a in 1:l) 
  #   {
  #     if(x[a] == 2)
  #     {
  #       data[, a] <- -data[, a]
  #     }
  #     if(x[a] == 3)
  #     {
  #       data[, a] <- n2s(data[, a])
  #     }
  #     patt[a] <- max(data[, a])
  #   }
  #   data2 <- data
  #   data2 <- rbind(data2, "pattern" = patt)
  #   
  #   #odległosci euklidesowe
  #   ed <- sapply(1:r, function(x)
  #   {
  #     EuclideanDistance(as.numeric(data2[x,]), as.numeric(data2[r+1, ]))
  #   })
  #   
  #   data2 <- cbind(data2, "ed" = c(ed, NA))
  #   d0 <- mean(ed) + 2*sd(ed)
  #   si <- sapply(1:r, function(x)
  #   {
  #     1-(ed[x]/d0)
  #   })
  #   
  #   data2 <- cbind(data2, "Si" = c(si, NA))
  #   
  #   
  #   
  #   return(data2)
  # }
  # 
  }
  
  
  
  ###############################################
  if(method == "hellwig")
  {
    patt <- NA
    
    
    #zamiana na stymulanty
    for (a in 1:length(x)) 
    {
      if(x[a] == 1)
      {
        patt[a] <- max(data_st[, a])
      }
      if(x[a] == 2)
      {
        patt[a] <- min(data_st[, a])
      }
      if(x[a] == 3)
      {
        m <- mean(data[, a])
        s <- sd(data[, a])
        patt[a] <- (nom[1] - m) / s
        nom <- nom[-1]
      }
    }
    data2 <- data_st
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
    
    data3 <- data2[-dim(data2)[1], ]
    nazwy <- rownames(data)
    tib <- as_tibble(data3)
    tib$nazwy2 <- nazwy
    tib <- tib %>%
      arrange(desc(si))
    tib <- as.data.frame(tib)
    rownames(tib) <- tib$nazwy2
    tib <- tib[, -dim(tib)[2]]
    tib <- rbind(tib, data2[dim(data2)[1], ])
    
    
    
    return(tib)
  }
  
  
  
}
