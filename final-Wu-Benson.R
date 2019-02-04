##Problem 1A
##HALF NORMAL DISTRIBUTION 
erf <- function(x){ 1 - 2 * pnorm(-sqrt(2) * ((x)/(sqrt(2))))}

closed_form = function(x){(erf(x))}

curve(closed_form(x),
      from=0, to=5, xlab="x", ylab="y", col = "orange")


#Problem 1B
n=1e6
x = rnorm(n, 0, 1)
SampleMean = abs(sum(x)/n)
Difference_in_means = numeric(B)
B=1e4
for (b in 1:B){
  
  XBoot=sample(x, size = n , replace = TRUE)
  XMeanBoot = abs(sum(XBoot)/n)
  
  Difference_in_means[b] = sqrt(n)*(XMeanBoot - SampleMean)
  
}

plot.ecdf(Difference_in_means)
curve(closed_form(x), add = TRUE, col = "orange")


#Problem 1C
#The limit  of sqrt(n)*abs(Xbar_n) as n->infinity points at the central limit theorem. When X is iid standard normal, sqrt(n)*(sample average of X - population mean) converges to a standard normal distribution 
#In our case, X is already iid standard normal, so we know the population mean is 0, so therefore, we know sqrt(n)*Xbar_n converges to normal.
#However, because of the absolutte value of X, we know that this is a half normal distribution, where it is a fold at the mean of an ordinary normal distribution with mean 0 according to wiki.

#The bootstrap in part B should be able to replicate the distribution of part A.
#However, it clearly fails when theta is 0 (which we have because X is iid standard normal) because the the absolute value makes it not smooth at the origin, so that is why the CDF starts past -1.



##Problem 2A
pval=numeric(8)

#took the Fisher.test code structure from lecture as allowed
#Study 1
SurgicalIntervention = 
matrix(c(9, 6, 39, 44),
       nrow = 2,
       dimnames = list(Groups = c("Treatment", "Control"),
                       Interest = c("Recurrence", "No Recurrence")))
pval[1]=fisher.test(SurgicalIntervention, alternative = "less")$p.value

#Study 2
SurgicalIntervention = 
matrix(c(15, 5, 55, 62),
         nrow = 2,
         dimnames = list(Groups = c("Treatment", "Control"),
                         Interest = c("Recurrence", "No Recurrence")))
pval[2]=fisher.test(SurgicalIntervention, alternative = "less")$p.value

#Study 3
SurgicalIntervention = 
  matrix(c(1, 4, 67, 65),
         nrow = 2,
         dimnames = list(Groups = c("Treatment", "Control"),
                         Interest = c("Recurrence", "No Recurrence")))
pval[3]=fisher.test(SurgicalIntervention, alternative = "less")$p.value

#Study 4
SurgicalIntervention = 
  matrix(c(5, 6, 54, 58),
         nrow = 2,
         dimnames = list(Groups = c("Treatment", "Control"),
                         Interest = c("Recurrence", "No Recurrence")))
pval[4]=fisher.test(SurgicalIntervention, alternative = "less")$p.value

#Study 5
SurgicalIntervention = 
  matrix(c(7, 2, 64, 72),
         nrow = 2,
         dimnames = list(Groups = c("Treatment", "Control"),
                         Interest = c("Recurrence", "No Recurrence")))
pval[5]=fisher.test(SurgicalIntervention, alternative = "less")$p.value

#Study 6
SurgicalIntervention = 
  matrix(c(8, 6, 29, 31),
         nrow = 2,
         dimnames = list(Groups = c("Treatment", "Control"),
                         Interest = c("Recurrence", "No Recurrence")))
pval[6]=fisher.test(SurgicalIntervention, alternative = "less")$p.value

#Study 7
SurgicalIntervention = 
  matrix(c(13, 8, 63, 67),
         nrow = 2,
         dimnames = list(Groups = c("Treatment", "Control"),
                         Interest = c("Recurrence", "No Recurrence")))
pval[7]=fisher.test(SurgicalIntervention, alternative = "less")$p.value

#Study 8
SurgicalIntervention = 
  matrix(c(9, 4, 47, 46),
         nrow = 2,
         dimnames = list(Groups = c("Treatment", "Control"),
                         Interest = c("Recurrence", "No Recurrence")))
pval[8]=fisher.test(SurgicalIntervention, alternative = "less")$p.value


##Problem 2B
ks.test(pval, punif)
#p-value = 0.006392
#H0: The distribution of pval is uniformly distributed under the null
#H1: The distribution of pval is not uniformly distributed under the null




##Problem 2C
#phi inverse as a function of 1-P_j
ls.test = function(m){
  sum=0
  for (i in 1:m){
    sum = sum + qnorm(1-pval[i], mean=0, sd=1)
  }
  sum = (1/sqrt(m))*sum
  return(sum)
}
#-3.224369

#Problem 2D


SurgicalIntervention <-
  array(c(9, 6, 39, 44,
          15, 5, 55, 62,
          1, 4, 67, 65,
          5, 6, 54, 58,
          7, 2, 64, 72,
          8, 6, 29, 31,
          13, 8, 63, 67,
          9, 4, 47, 46),
        dim = c(2, 2, 8),
        dimnames = list(
          Groups = c("Treatment", "Control"),
          Interest = c("Recurrence", "No Recurrence"),
          Study_number = c("1", "2", "3", "4", "5", "6", "7", "8")))
mantelhaen.test(SurgicalIntervention, exact = TRUE, alternative = "less")
#p-value = 0.9971





###Problem 3
poly.fit = function(x, y, stop = 0.05){
  
  p = 0
  New_R_sq = 0
  Old_R_sq = 0
  R_sq_difference = 1
  
  while (R_sq_difference > stop){
    p=p+1
    fit = lm(y~poly(x,p, raw=TRUE))
    New_R_sq = summary(fit)$r.squared
    R_sq_difference = New_R_sq - Old_R_sq
    Old_R_sq = New_R_sq
   
  }
  return(as.numeric(summary(fit)$coefficients[,1]))
}

library("MASS")
x=steam$Temp
y=steam$Press
poly.fit(x,y,stop = 0.05)

#Beta0_hat = -7.150413355, Beta1_hat = 3.368273275, Beta2_hat = -0.106072872, Beta3_hat = 0.001508421 









