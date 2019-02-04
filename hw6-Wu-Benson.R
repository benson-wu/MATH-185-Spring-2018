########################################################################################################################################
#1
#We are trying to build a test that is like Tukey's test which compares means, but in this case we are going to use the wilcox test to compare the distributions of different
#types of smokers, so we will build a function that will store the wilcox statistic (mean ranks) that we get from the wilcox test in a matrix. Then we need to standardize it to remove the means.
#then we will use a bootstrap to boostrap the statistic values then do the usual Dobs>=Dorig / B+1 to get p values, then we will store the p values in a matrix that will compare 
#different types of smokers.
rankHSD = function(y,g,B=999){
  c=length(levels(g))
  lev=levels(g)
  morig=matrix(0,nrow=c,ncol=c,byrow=T)
  for(i in 1:(c-1)){
    for (j in (i+1):c){
      morig[j,i]=as.numeric(wilcox.test(y[g==lev[j]], y[g==lev[i]], alternative="greater")$statistic)
    }
  }
  #standardize
  for (level in levels(g)){
    y[g==level] = scale(y[g==level], center=T, scale=F)
  }
  countmatrix=matrix(0, nrow=c, ncol=c,byrow=T)
  
  for (b in 1:B){
    mboot=matrix(0, nrow=c, ncol=c, byrow=T)
    newvec=0
    for (level in levels(g)){
      newvec=c(newvec, sample(y[g==level], length(y[g==level]), replace=TRUE))
    }
    for (i in 1:(c-1)){
      for (j in (i+1):c){
        mboot[j,i] = as.numeric(wilcox.test(newvec[g==lev[j]], newvec[g==lev[i]], alternative = "greater")$statistic)
        if (mboot[j,i] >=morig[j,i]){countmatrix[j,i] = countmatrix[j,i]+1
        }
      }
    }
  }
  return((countmatrix+1)/(B+1))
}

y=stack(smokers)$values
g=stack(smokers)$ind
rankHSD(y,g,B)

#      [,1]  [,2]  [,3]  [,4]
#[1,] 0.001 0.001 0.001 0.001
#[2,] 0.608 0.001 0.001 0.001
#[3,] 0.194 0.097 0.001 0.001
#[4,] 0.050 0.012 0.066 0.001

#a row represents a group and a column represents another group
#H0 = the groups have the same mean
#H1 = the groups don't have the same mean




########################################################################################################################################
#2
install.packages("testforDEP")
require(testforDEP)

hoeff.test=function(x,y,B=999){
  len=length(x)
  D.obs=hoeffd(cbind(x,y))$max[2,1]
  count=0
  
  for(b in 1:B){
    y.perm=sample(y,len,replace=F)
    x.perm=sample(x,len,replace=F)
    D.perm=hoeffd(cbind(x,y.perm))$max[2,1]
    if(D.perm>=D.obs){count=count+1}
  }
  return((count+1)/(B+1))
}



########################################################################################################################################
#3
#All 82 games
#Kevin Durant
x=c(27, 30, 37, 20, 39, 27, 22, 28, 18, 29, 30, 23, 33, 14, 28, 29, 28, 25, 39, 20, 20, 16, 21, 21, 22, 27, 15, 34, 22, 26, 32, 36, 22, 19, 21, 30, 27, 28, 28, 25, 21, 40, 32, 15, 27, 33, 23, 33, 18, 26, 10, 22, 24, 34, 25, 21, 25, 0, 27, rep(0, 20), 16, 16, 29) 

#Stephen Curry
y=c(26, 23, 28, 28, 21, 13, 46, 24, 33, 30, 35, 16, 20, 22, 31, 24, 34, 25, 28, 31, 13, 19, 26, 17, 22, 30, 8, 19, 25, 15, 25, 15, 28, 14, 22, 35, 40, 30, 24, 24, 20, 24, 24, 27, 21, 28, 43, 0, 39, 29, 35, 13, 18, 26, 11, 13, 35, 27, 19, 25, 23, 31, 24, 23, 26, 0, 29, 25, 28, 23, 17, 27, 21, 32, 29, 24, 42, 19, 42, 0, 28, 20)

#With all 82 games


hoeff.test(x, y, B)
#p=.736
cor.test(x,y, method="spearman")
#p=0.731



#Only games where both played
#Kevin Durant's points ommitting games not played
xnew=x[-which(x==0 | y==0)]
#Stephen Curry's points ommitting games not played
ynew=y[-which(x==0 | y==0)]

B=999
x=xnew
y=ynew
hoeff.test(x,y,B)
#p=.150485
cor.test(x,y, method="spearman")
#p=0.6437



