#1a
load("C:/Users/Benson/Documents/R/smokers.rda")

B=10000
bootBetweenSumSquares = function(y, g, B = 1e4){
  count = 0 
  c = length(levels(g))

  #original SST
  sstpart=numeric(c)
  ind=1

  for (level in levels(g)){
  sstpart[ind] = length(y[g==level])*(mean(y[g==level])-mean(y))^2
  ind = ind+1
    }
  
  sstorig=sum(sstpart)

  #standardize with scale
  for (level in levels(g)){
  y[g==level]=scale(y[g==level], center = TRUE, scale = FALSE)
    }

  #bootstrap
  for (b in 1:B){
  newvec=numeric(0)
  for (level in levels(g)){
    newvec=c(newvec, sample(y[g==level], length(y[g==level]), replace=TRUE))
    }
  
  
    sstpartboot=numeric(c)
    index=1
    for(i in levels(g)){
      sstpartboot[index]=length(newvec[g==level])*mean(newvec[g==level])-mean(newvec)^2
      index=index+1
        }
    sstboot=sum(sstpartboot)
    if(sstboot >= sstorig){
      count = count +1
      }

  }      
     return((count+1)/(B+1))
  }

y=stack(smokers)$values
g=stack(smokers)$ind


bootBetweenSumSquares(y, g, B)

#9.999e-05





#1b
y=stack(smokers)$values
g=stack(smokers)$ind
B=10000
bootWelchF = function(y, g, B){
  statorig=as.numeric(oneway.test(y~g)$statistic, var.equal=FALSE)
  count=0
  for(level in levels(g)){
    y[g==level] = scale(y[g==level], center=TRUE, scale=FALSE)
    
  }
  for (b in 1:B){
    newvec=numeric(0)
    for (level in levels(g)){
      newvec=c(newvec, sample(y[g==level], length(y[g==level]), replace = TRUE))
      
    }
    statboot=as.numeric(oneway.test(newvec~g)$statistic, var.equal=FALSE)
    if(is.na(statboot==TRUE)){
      statboot=rbinom(1,1,0.05)*(statorig+1)}
    if (statboot>=statorig){count=count+1}
    
  }
  return((count+1)/(B+1))
}

bootWelchF(y, g, B)
#0.05309469

#1c

B=10000
level=0.05
range = seq(0, 2, 0.2)
len=length(range)
par(mfrow=c(2,2))
size=c(10, 100)
sigmas=c(1,3)

#1st for loop accounts for different types of sample sizes that you will use
for (n in size){

  
  #2nd for loop for sigmas
  for(sigma in sigmas){
    fracrejectSS=numeric(len)
    fracrejectWelch=numeric(len)
  
    #3rd for loop that accounts for the range of mu's
    for (mu in range){
      a=rnorm(n, mu, sigma)
      b=rnorm(n, 0 , 1)
      countSS=0
      countWelch=0
      
      dat = as.data.frame(cbind(a,b))
      stack_dat = stack(dat)
      
      y=stack_dat$values
      g=stack_dat$ind
      
    
      #4th for loop that runs the tests 200 times
      for (n in 1:200){
        SS=bootBetweenSumSquares(y,g,B)
        Welch=bootWelchF(y,g,B)
        
        #counts the number of rejections
        if (SS<=level){countSS=countSS+1}
        if (Welch<=level){countWelch=countWelch+1}
      }
    #gives fraction of rejections 
    fracrejectSS[which(range==mu)]=countSS/200
    fracrejectWelch[Which(range==mu)]=countWelch/200
    }
  plot(fracrejectSS, pch=16, main = "Power of tests", xlab = "mu (from 0 to 2 by increments of 0.2")
  lines(fracrejectSS, col="orange", add=TRUE)
  lines(fracrejectWelch, col="green", add=TRUE)
  }
}

#code is taking too long to run, so I cannot get plots to make comments

#1d
B=10000
level=0.05
range = seq(0, 2, 0.2)
len=length(range)
par(mfrow=c(2,2))
size=c(10, 100)
sigmas=c(1,3)

#1st for loop accounts for different types of sample sizes that you will use
for (n in size){
  
  
  #2nd for loop for sigmas
  for(sigma in sigmas){
    fracrejectSS=numeric(len)
    fracrejectWelch=numeric(len)
    
    #3rd for loop that accounts for the range of mu's
    for (mu in range){
      a=rnorm(n, mu, sigma)
      b=rnorm(n, mu+1, sigma)
      c=rnorm(n, 0 , 1)
      d=rnorm(n, mu+2, sigma)
      countSS=0
      countWelch=0
      
      dat = as.data.frame(cbind(a,b,c,d))
      stack_dat = stack(dat)
      
      y=stack_dat$values
      g=stack_dat$ind
      
      
      #4th for loop that runs the tests 200 times
      for (n in 1:200){
        SS=bootBetweenSumSquares(y,g,B)
        Welch=bootWelchF(y,g,B)
        
        #counts the number of rejections
        if (SS<=level){countSS=countSS+1}
        if (Welch<=level){countWelch=countWelch+1}
      }
      #gives fraction of rejections 
      fracrejectSS[which(range==mu)]=countSS/200
      fracrejectWelch[Which(range==mu)]=countWelch/200
    }
    plot(fracrejectSS, pch=16, main = "Power of tests", xlab = "mu (from 0 to 2 by increments of 0.2")
    lines(fracrejectSS, col="orange", add=TRUE)
    lines(fracrejectWelch, col="green", add=TRUE)
  }
}





#2

y=stack(smokers)$values
g=stack(smokers)$ind
 
multipleksstat=function(y, g){
  level=levels(g)
  len=length(level)
  s=0
  for(i in 1:(len-1)){
    for(j in (i+1):len){
      test=ks.test(y[g==level[i]], y[g==level[j]])
      s=max(s, test$statistic)
          }
        }
    return(s)
  }


#This function generalizes the ks test to multiple groups instead of just two by taking the maximum of two groups. 
#whicever group is the maximum, it sort of moves on to the "next level" in order to be compared to another group through a ks test.
#Then you keep running this until 


#second part
B=10000
oneway.ks.test=function(y,g,B){
  origstat=multipleksstat(y,g)
  count=0
  for(b in 1:B){
    y.boot=sample(y, length(g), replace=TRUE)
    boot.stat=multipleksstat(y.boot, g)
    if (boot.stat>=origstat){count=count+1}
  }
  return((count+1)/(B+1))
}

#0.09749025



