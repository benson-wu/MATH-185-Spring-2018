#Benson Wu A13171365
#Problem 1A
chisq.sim.hist=function(n, k, B){
  x = c(1:k)
  D = numeric(B) 
  for(i in 1:B){
    for(j in 1:k){
      sample = sample(x, size=n, replace=TRUE)
      D[i]=D[i]+(sum(sample==k)-(n/k))^2/(n/k)
    }
  }
  hist(D, probability=TRUE)
  curve(dchisq(x, k-1), add=TRUE)
}

#Problem 1B
par(mfrow=c(2,2))
for(n in c(10,20,50,100)){
  chisq.sim.hist(n, 5, 1e4)
}

#Problem 1C
chisq.sim.critical=function(n, k, B){
  x = c(1:k)
  D = numeric(B) 
  for(i in 1:B){
    for(j in 1:k){
      sample = sample(x, size=n, replace=TRUE)
      D[i]=D[i]+(sum(sample==k)-(n/k))^2/(n/k)
    }
  }
quantile(D, probs=0.95)
}

#Problem 1D
qchisq(0.95, 4)
y=seq(10,100, by=10)
U=numeric(10)
for(t in y){
  U[t/10]=chisq.sim.critical(t, 5, 1e4)
} 
vector1 = rep(qchisq(0.95, 4), 10)
plot(vector1, type = "o")
lines(U, type = "o")



#Problem 2
setwd("C:/Users/Benson/Desktop")
Natality = read.table("Natality.txt", header = TRUE)

MaleBirths = subset(Natality, Gender=="Male", select = "Births") 
FemaleBirths = subset(Natality, Gender=="Female", select = "Births") 

Counts = matrix(NA, 2, 12)
Counts[1,] = MaleBirths$Births
Counts[2,] = FemaleBirths$Births
Counts = as.data.frame(Counts)
names(Counts) = 1:12
row.names(Counts) = c('Male', 'Female')
Counts

Counts = as.matrix(Counts)
barplot(Counts, legend = TRUE, args.legend=list(x="top"))
barplot(Counts, beside=TRUE, legend=TRUE, args.legend=list(x="top"))

#Null Hypothesis: Males and Females are equally likely to be born any month of the year. 
#Alternative Hypotheseis: Males and Femals are not equally likely to be born any month of the year

#I will use significance level of 0.05 and conduct a chi-square test of homogeneity
chisq.test(Counts)

#X-squared = 18.866, df = 11, p-value = 0.06355
#Since the p-value=0.06355, and 0.06355 is larger than 0.05, then we
#cannot reject the null hypothesis, therefore
#Males and Females are equally likely to be born any month of the year. 