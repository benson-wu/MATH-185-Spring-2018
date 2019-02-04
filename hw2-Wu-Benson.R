#1
chisqBootTest=function(tab, B=10000){

  Dobserve=chisq.test(tab)$stat 
  
  numrow=nrow(tab)
  numcol=ncol(tab)
  
  
  totalcounts=sum(tab[,]) 
  
  
  rows=numeric(numrow)
  vecrow=numeric(0)
  for (i in 1:numrow){rows[i]=sum(tab[i,])
  vecrow=c(vecrow, rep.int(i, times=rows[i]))
  }
  
  
  columns=numeric(numcol)
  veccol=numeric(0)
  for (j in 1:numcol){columns[j] = sum(tab[,j])
  veccol=c(veccol, rep.int(j, times=columns[j]))
  }
  

}

foodvector = c(15, 24, 25, 239, 7, 14, 8, 273)
food = matrix(foodvector, nrow=2, ncol=4, byrow=TRUE )

chisqBootTest(food, B=10000)
chisq.test(food, correct=FALSE) 




#2a
B=10000
True.mean.fraction = function(n, B){
  count = 0
  for (b in 1:B){
    sample=rnorm(n)
    lower=mean(sample)+qt(.025, df = n-1)*sd(sample)/sqrt(n)
    upper=mean(sample)+qt(.975, df = n-1)*sd(sample)/sqrt(n)
    if ((lower<=0)&&(upper>=0)==TRUE){count=count+1}
  }
  
  result=count/B
  result
}
True.mean.fraction(15,B)
True.mean.fraction(150,B)

#2b
install.packages("rmutil")
require(rmutil)
B=10000
True.mean.fraction = function(n, B){
  count = 0
  for (b in 1:B){
    sample=rlaplace(n, m=0, s=1/sqrt(2))
    lower=mean(sample)+qt(0.25, df=n-1)*sd(sample)/sqrt(n)
    upper=mean(sample)+qt(0.75, df=n-1)*sd(sample)/sqrt(n)
    if ((lower<=0)&&(upper>=0)==TRUE){count=count+1}
    
  }
  result=count/B 
  result
}

True.mean.fraction(15,B)
True.mean.fraction(150,B)


#2.c
B=10000
True.mean.fraction = function(n, B){
  count = 0
  for (b in 1:B){
    sample=rexp(n, rate=1)
    lower=mean(sample)+qt(.025, df = n-1)*sd(sample)/sqrt(n)
    upper=mean(sample)+qt(.975, df = n-1)*sd(sample)/sqrt(n)
    if ((lower<=1)&&(upper>=1)==TRUE){count=count+1}
    
  }
  result=count/B 
  result
}

True.mean.fraction(15,B)
True.mean.fraction(150,B)








