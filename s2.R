library(plyr)
library(dplyr)
library(TSdist)
library(plyr)
piwo <- read.csv("piwo.csv", sep = ";")
rownames(piwo) <- piwo[,1]
piwo <- piwo[,-1]

piwo2 <- scale(piwo)
piwo2 <- as.data.frame(piwo2)
a1 <- median(piwo2$zawartosc.alk)
a2 <- min(piwo2$cena)
a3 <- max(piwo2$dostepnosc)
a4 <- max(piwo2$znajomosc)
a5 <- max(piwo2$preferencje)

xd <- c(a1, a2, a3, a4, a5)
# xd <- data.frame(a1, a2, a3, a4, a5)1
#names(xd) <- names(piwo)
piwo2 <- rbind(piwo2, "wowowo" = xd)
rownames(piwo2)[21] <- "wzorzec"

ed <- function(df)
{
  l <- dim(df)[1] - 1
  a <- sapply(1:l, function(x)
  {
    EuclideanDistance(as.numeric(df[x,]), as.numeric(df[l+1, ]))
  })
  return(c(a, NA))
}

piwo2 <- cbind(piwo2, ed(piwo2))

mH_w <- function(d)
{
  #d <- as.data.frame(d)
  l <- length(d)
  d0 <- mean(d) + 2*sd(d)
  si <- sapply(1:l, function(x)
  {
    1-(d[x]/d0)
  })
  return(si)
}




piwo2[,7] <- c(mH_w(piwo2[1:20,6]), NA)
piwo2[,8] <- rownames(piwo2)

a <- piwo2 %>%
  arrange(V7)

a <- as.data.frame(a)
rownames(a) <- a$V8
a <- a[,-8]
a

b <- c(1, 2, 1, 3 , 1)
b <- as.factor(b)
mapvalues(b, from = c(1, 2, 3), to = c("a", "b", "c"))



f <- factor(c(3, 2, 1, 1, 1), levels = c(1, 2, 3))
foo(piwo, f)








