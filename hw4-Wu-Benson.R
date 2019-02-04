#1
locationScaleGOF = function(X,Y){
  #The logic behind this code is that the function locationScaleGof
  #will take two samples, X and Y, and then create empty vectors the size of 
  #each sample.  
  normalX = numeric(length(X))
  normalY=numeric(length(Y))
  
  
  #Then you want to standardize the data separately in two different for loops then 
  #put the standardized data into the empty vectors previously created. 
  for (x in X){
  st.x = (x-mean(X)/sd(X))
  normalX[x]=st.x
  }
  
  
  for (y in Y){
    st.y = (y-mean(Y)/sd(Y))
    normalY[y]=st.y
  }

  #Then run a Kolmogorov-Smirnov Test with the new standardized samples and if 
  #p<0.05, you reject the null hypothesis that states that the two samples have 
  #the same distribution and we conclude that the samples don't have the same distributions
return(ks.test(normalX, normalY)) 
    
}




  
  
#2a
B=200
level=0.1
range = seq(0, 2, 0.05)
len=length(range)
par(mfrow=c(2,2))
size=c(19, 20, 50, 100)

#1st for loop accounts for different types of sample sizes that you will use
for (n in size){
  fracrejectt=numeric(len)
  fracrejectw=numeric(len)
  
  #2nd for loop that accounts for the range of mu's
  for (mu in range){
    countt=0
    countw=0
    
    #3rd for loop that generates 200 pvalues for the range of mu's
    for (b in 1:B){
    x=rnorm(n, mu, 1)
    y=rnorm(n, 0 , 1)
    pvalt=t.test(x,y, alternative = "greater", var.equal=TRUE)$p.value
    pvalw=wilcox.test(x, y, alternative = "greater")$p.value
    
    #counts the number of rejections
    if (pvalt<=level){countt=countt+1}
    if (pvalw<=level){countw=countw+1}
    }
    #gives fraction of rejections 
    fracrejectt[20*mu+1]=countt/B
    fracrejectw[20*mu+1]=countw/B
  }
  plot(fracrejectt, pch=16, main = "fractions of rejections vs mu", xlab = "mu (from 0 to 2 by increments of 0.05")
  lines(fracrejectt, col="orange", add=TRUE)
  lines(fracrejectw, col="green", add=TRUE)
}
  
  
#2b
B=200
level=0.1
range = seq(0, 2, 0.05)
len=length(range)
par(mfrow=c(2,2))
size=c(19, 20, 50, 100)

for (n in size){
  fracrejectt=numeric(len)
  fracrejectw=numeric(len)
  
  for (mu in range){
    countt=0
    countw=0
    
    for (b in 1:B){
      x=rcauchy(n, mu, 1)
      y=rcauchy(n, 0 , 1)
      pvalt=t.test(x,y, alternative = "greater", var.equal=TRUE)$p.value
      pvalw=wilcox.test(x, y, alternative = "greater")$p.value
      if (pvalt<=level){countt=countt+1}
      if (pvalw<=level){countw=countw+1}
    }
    fracrejectt[20*mu+1]=countt/B
    fracrejectw[20*mu+1]=countw/B
  }
  plot(fracrejectt, pch=16, main = "fractions of rejections vs mu", xlab = "mu (from 0 to 2 by increments of 0.05")
  lines(fracrejectt, col="orange", add=TRUE)
  lines(fracrejectw, col="green", add=TRUE)
}




  
  
  
  
#3
setwd("C:/Users/Benson/Desktop")   
install.packages("cramer")
library(cramer)
Calcium = read.table("calciumgood.dat.txt", header = FALSE)
Calcium = as.matrix(Calcium)

matrixmen = subset(Calcium, Calcium[,3]==1)
matrixwomen=subset(Calcium, Calcium[,3]==2)

MaleCalcium=as.numeric(matrixmen[,8])

FemaleCalcium=as.numeric(matrixwomen[,8])

FemaleCalcium = na.omit(FemaleCalcium) 
FemaleCalcium = as.numeric(FemaleCalcium)


cramer.test(MaleCalcium, FemaleCalcium, conf.level =0.95, replicates=2000, sim="ordinary", just.statistic = FALSE)

#critical value for confidence level  95 % :  0.1896459 
#observed statistic  0.5556059 , so that
#hypothesis ("x is distributed as y") is  REJECTED .
#estimated p-value =  0 
