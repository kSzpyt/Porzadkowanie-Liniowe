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
  if(!is.na(nom) & length(which(x == 3)) != length(nom))
  {
    stop("niezgodna ilość nominant")
  }
  #zamiana nominanty na stymulante
  

  #zamiana nominanty na stymulante
  n2s <- function(v, nom2)
  {
    aaa <- v
    l <- length(aaa)
    v2 <- NULL
    for (x in 1:l) {
      v2[x] <- (-abs(aaa[x] - nom2))
    }
    return(v2)
  }
  
  #jeżeli użytkownik nie poda wartości nominanty brana pod uwagę będzie mediana
  if(is.na(nom))
  {
    ind <- which(x == 3)
    nom <- sapply(ind, function(x){
      median(data[, x])
    })
  }
  
  #wymiary
  l <- dim(data)[2]#collumns
  r <- dim(data)[1]#rows

  ###############################################
  if(method == "hellwig")
  {
    #standaryzacja
    data_st <- scale(data)
    data_st <- as.data.frame(data_st)
    patt <- NA
    #wyznaczenie wzorca
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
    
    #sortowanie
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
  
  if(method == "mss")
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
        data[, a] <- n2s(data[, a], nom[1])
        nom <- nom[-1]
      }
    }
    
    #standaryzacja
    data_st <- scale(data)
    data_st <- as.data.frame(data_st)
    
    s_sums <- sapply(1:r, function(x){
      sum(data_st[x,])
    })
    
    #standaryzajca
    # s_sums_st <- sapply(1:r, function(x){
    #   (s_sums[x] - min(s_sums))/(max(s_sums[x] - min(s_sums)))
    # })
    
    maximum <- max(s_sums)
    minimum <- min(s_sums)
    s_sums_st <- scale(s_sums, center = minimum, scale=maximum-minimum)
    
    data_st <- cbind(data_st, s_sums, "wsk" = s_sums_st)
    
    
    nazwy <- rownames(data)
    tib <- as_tibble(data_st)
    tib$nazwy2 <- nazwy
    tib <- tib %>%
      arrange(desc(wsk))
    tib <- as.data.frame(tib)
    rownames(tib) <- tib$nazwy2
    tib <- tib[, -dim(tib)[2]]
    
    
    return(tib)
  }
    

  
  
}
