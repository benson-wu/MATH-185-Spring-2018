#hw3
#Problem 1a
sd.bootCI = function(x, level=0.95, B=2000){
  
  SDx = sd(x)
  SDboot = rep(0,B)
  alpha = 1-level
  n=length(x)
  
  for (b in 1:B){
    X.boot = sample(x, size = n, replace = TRUE)
    SDboot[b]=sd(X.boot)
  }
  
  
  
    SDbootlarge = quantile(SDboot, (1-(alpha/2)))
    SDbootsmall = quantile(SDboot, (alpha/2))
    
    lower = (2*SDx-SDbootlarge)
    upper = (2*SDx-SDbootsmall)
    return(as.numeric(c(lower, upper)))
}

#Problem 1b
sd.bootCI.student = function(x, level=0.95, B=2000){
  
  thetahat = sd(x)
  alpha = 1-level
  len = length(x)
  theta=rep(0,B)
  t.ratio=rep(0,B)
  
  for (b in 1:B){
    generate = sample(x, size=len, replace=TRUE)
    theta[b] = sd(generate)
    
    mugenerate=mean(generate)
    sumfour=0 
    
      
    for (i in 1:len){
      sumfour=sumfour+(generate[i] - mugenerate)^4
    }
      mufour=sumfour/len
      sigmafour=(sd(generate))^4
      se=sqrt((1/len)+(mufour-((len-3)*sigmafour/(len-1))))
      t.ratio[b]=theta[b]-thetahat/se
    
  }
  tlarge=quantile(t.ratio, 1-alpha/2)
  tsmall=quantile(t.ratio, alpha/2)
  seboot=sd(theta)
  lowerbound=thetahat-tlarge*seboot
  upperbound=thetahat-tsmall*seboot
  return(as.numeric(c(lowerbound, upperbound)))

}
 



#Problem 1c

##sd.bootCI repetitions with different n's

#n=10
count=0
length=numeric(200)
for (b in 1:200){
  x=rexp(10, rate=1)
  CI = sd.bootCI(x, level=0.95, B=2000)
  lowerboundpivotal= CI[1]
  upperboundpivotal= CI[2]
  if (lowerboundpivotal<= 1 & 1<= upperboundpivotal) {
    count = count + 1
  }
  
  
  length[b]=upperboundpivotal-lowerboundpivotal

  }

fraction_of_SD_in_CI = count/200
fraction_of_SD_in_CI
#around high .6's - low .7's

hist(length)


#n=20
count=0
length=numeric(200)
for (b in 1:200){
  x=rexp(20, rate=1)
  CI = sd.bootCI(x, level=0.95, B=2000)
  lowerboundpivotal= CI[1]
  upperboundpivotal= CI[2]
  if (lowerboundpivotal<= 1 & 1<= upperboundpivotal) {
    count = count + 1
  }
  
  
  length[b]=upperboundpivotal-lowerboundpivotal
  
}

fraction_of_SD_in_CI = count/200
fraction_of_SD_in_CI
#around mid .7's - high .7's

hist(length)


#n=50
count=0
length=numeric(200)
for (b in 1:200){
  x=rexp(50, rate=1)
  CI = sd.bootCI(x, level=0.95, B=2000)
  lowerboundpivotal= CI[1]
  upperboundpivotal= CI[2]
  if (lowerboundpivotal<= 1 & 1<= upperboundpivotal) {
    count = count + 1
  }
  
  
  length[b]=upperboundpivotal-lowerboundpivotal
  
}

fraction_of_SD_in_CI = count/200
fraction_of_SD_in_CI
#around mid .8's - high .8's

hist(length)


#n=100
count=0
length=numeric(200)
for (b in 1:200){
  x=rexp(100, rate=1)
  CI = sd.bootCI(x, level=0.95, B=2000)
  lowerboundpivotal= CI[1]
  upperboundpivotal= CI[2]
  if (lowerboundpivotal<= 1 & 1<= upperboundpivotal) {
    count = count + 1
  }
  
  
  length[b]=upperboundpivotal-lowerboundpivotal
  
}

fraction_of_SD_in_CI = count/200
fraction_of_SD_in_CI
#around high .8's to low .9's

hist(length)

#n=200
count=0
length=numeric(200)
for (b in 1:200){
  x=rexp(200, rate=1)
  CI = sd.bootCI(x, level=0.95, B=2000)
  lowerboundpivotal= CI[1]
  upperboundpivotal= CI[2]
  if (lowerboundpivotal<= 1 & 1<= upperboundpivotal) {
    count = count + 1
  }
  
  
  length[b]=upperboundpivotal-lowerboundpivotal
  
}

fraction_of_SD_in_CI = count/200
fraction_of_SD_in_CI
#generally around .95, as should happen

hist(length)






##sd.bootCI.student repetitions with different n's


#n=10
count=0
length=numeric(200)
for (b in 1:200){
  x=rexp(10, rate=1)
  CI = sd.bootCI.student(x, level=0.95, B=2000)
  lowerboundpivotal= CI[1]
  upperboundpivotal= CI[2]
  if (lowerboundpivotal<= 1 & 1<= upperboundpivotal) {
    count = count + 1
  }
  
  
  length[b]=upperboundpivotal-lowerboundpivotal
  
}

fraction_of_SD_in_CI = count/200
fraction_of_SD_in_CI
#around mid .5's

hist(length)


#n=20
count=0
length=numeric(200)
for (b in 1:200){
  x=rexp(20, rate=1)
  CI = sd.bootCI.student(x, level=0.95, B=2000)
  lowerboundpivotal= CI[1]
  upperboundpivotal= CI[2]
  if (lowerboundpivotal<= 1 & 1<= upperboundpivotal) {
    count = count + 1
  }
  
  
  length[b]=upperboundpivotal-lowerboundpivotal
  
}

fraction_of_SD_in_CI = count/200
fraction_of_SD_in_CI
#around mid .6's - low .7's

hist(length)


#n=50
count=0
length=numeric(200)
for (b in 1:200){
  x=rexp(50, rate=1)
  CI = sd.bootCI.student(x, level=0.95, B=2000)
  lowerboundpivotal= CI[1]
  upperboundpivotal= CI[2]
  if (lowerboundpivotal<= 1 & 1<= upperboundpivotal) {
    count = count + 1
  }
  
  
  length[b]=upperboundpivotal-lowerboundpivotal
  
}

fraction_of_SD_in_CI = count/200
fraction_of_SD_in_CI


hist(length)


#n=100
count=0
length=numeric(200)
for (b in 1:200){
  x=rexp(100, rate=1)
  CI = sd.bootCI(x, level=0.95, B=2000)
  lowerboundpivotal= CI[1]
  upperboundpivotal= CI[2]
  if (lowerboundpivotal<= 1 & 1<= upperboundpivotal) {
    count = count + 1
  }
  
  
  length[b]=upperboundpivotal-lowerboundpivotal
  
}

fraction_of_SD_in_CI = count/200
fraction_of_SD_in_CI


hist(length)

#n=200
count=0
length=numeric(200)
for (b in 1:200){
  x=rexp(200, rate=1)
  CI = sd.bootCI(x, level=0.95, B=2000)
  lowerboundpivotal= CI[1]
  upperboundpivotal= CI[2]
  if (lowerboundpivotal<= 1 & 1<= upperboundpivotal) {
    count = count + 1
  }
  
  
  length[b]=upperboundpivotal-lowerboundpivotal
  
}

fraction_of_SD_in_CI = count/200
fraction_of_SD_in_CI
#generally around .95, as should happen

hist(length)

  


#2a
install.packages("MASS")
library("Mass")
x = geyser$duration
sd.bootCI(x, level=0.90, B=2000)
#0.6668113 , 1.1833853

sd.bootCI.student(x, level=0.90, B=2000)
#0.8352614 , 1.0882742

#2b
install.packages("boot")
library("MASS")
library("boot")

x = geyser$duration
SDfunc = function(x,i){
  x2=x[i,]
  return(sd(x2$duration))
     }

b = boot(data = geyser, SDfunc, R = 2000)
boot.ci(b, conf=0.90, type = "norm")

#Intervals : 
#  Level      Normal        
#90%   ( 1.109,  1.192 ) 







#3
install.packages("KScorrect")
require(MASS)
x = geyser$waiting

library(KScorrect)
Lc <- LcKS(x, "pexp" , nreps=2000)
Lc$p.value
#p-value is 0.0004997501
#let alpha be 0.05
#H0: The waiting times are exponentially distrubted
#H1: The waiting times are not exponentially distributed
#Since the p-value from the test is 0.0004997501 and our alpha is 0.05, 
#then we reject the null hypothesis and say that the waiting times
#are not exponentially distributed



