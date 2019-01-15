#markov plot
setwd("E:/Saloni/Courtship")


library(ggplot2)
library(tidyr)
library(dygraphs)
library(grid)
library(reshape2)
library(dplyr)
library(XLConnect)
library(markovchain)
library(diagram)
library(gridExtra)

my_data1 <-readWorksheetFromFile("courtship.xlsx", sheet=1, startRow = 2, endCol = 11)
a1 <- ncol(my_data1)

mlist <- list()
for (j in 1:a1){
y <- my_data2[,j+1]
x <- matrix(unlist(y), ncol = 1, byrow = TRUE)
p <- matrix(nrow = 6, ncol = 6, 0)
for (t in 1:(length(x) - 1)) p[x[t], x[t + 1]] <- p[x[t], x[t + 1]] + 1
for (i in 1:6) p[i, ] <- p[i, ] / sum(p[i, ])
fmat <- t(p)
mlist[[j]]=fmat
}

flist <- do.call(cbind, mlist)
flist <- array(flist, dim=c(dim(mlist[[1]]), length(mlist)))
tmA <-rowMeans(flist, dim = 2,na.rm = TRUE)

stateNames <- c("Chase/Orientation","Singing","Tapping","Licking","Not Courting","Copulation")
row.names(tmA) <- stateNames; colnames(tmA) <- stateNames
tmA <- round(tmA, 3) 

plotmat(tmA,pos = c(1,2,2,1), curve= 0.3,box.prop = 0.5,box.size= 0.08, box.col = c("light blue","pink","gold","darkolivegreen1","white","tomato"),self.lwd = 
          0.003, self.cex = 0.4,self.shifty = NULL, self.arrpos = NULL,main = "Markov Diagram(FOXP3955)",arr.width=0.3, lwd=0.5, my=-0.05,dtext=0.2)

grid.table(tmA)

