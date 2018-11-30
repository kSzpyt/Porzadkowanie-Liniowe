library(dplyr)
piwo <- read.csv("piwo.csv", sep = ";")
rownames(piwo) <- piwo[,1]
piwo <- piwo[,-1]

piwo2 <- scale(piwo)
piwo2 <- as.data.frame(piwo2)
str(piwo2)

piwo2$cena <- -piwo2$cena

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

n2s(piwo2$zawartosc.alk)




