

# This is the actual predictive posterior:


k<-length(Data)
integrand <- function(r) {
  i<-1
  t<-0
  while(i<=k){
    t<-t+lgamma(r+Data[i])
    i<-i+1
  }
  return(r^(d2-1)*exp(-d1*r)*exp(
    lgamma(Y+r)-lgamma(r)+t-8*lgamma(r)+lgamma(8*r+A)+
      lgamma(r+1)-lgamma(8*r+sum(Data)+A+B)-lgamma(r+Y+2)+500))
} 



# Calculate Posterior Predictive for y in 30000

ypred <- vector(mode = "numeric", length = 30001)

for(Y in seq(0, 30000)) {
  
  current <- integrate(integrand, 0, Inf)
  
  ypred[Y] <- current$value
}

y.pred.stand <- ypred/sum(ypred)

# y.pred.stand is the true PMF for all values y 1:30000



### Expected Value:

sum(seq(0,30000)*y.pred.stand)

# 14.25167



### Mode

seq(0,30000)[y.pred.stand==max(y.pred.stand)]

# probability is strictly decreasing as y increases; not meaningful here


### Plot

x <- seq(0,30)

pp <- y.pred.stand[x+1]

plot(x,pp, type="s", lty=1, xlab = "Future Observation", ylab = "Prob. of Observation",
  main = "Predictive Posterior Plot")


### Highest Posterior Density Interval (HPD)

count <- 0

y <- -1

while (count <= .95){
  
  y <- y+1
  count <- count + y.pred.stand[y+1]
  
}

# count = .95 when y reaches 31




