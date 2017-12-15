

Data <- c(11,7,4,62,13,6,396,64)

# The goal is to model this data as Negative Binomial (p,r) .


#####
#####
##### Prior distributions for p, r #####


# estimate p for our data

p.data <- mean(Data)/var(Data)  # 0.00392449

# estimate r for our data

0.00392449/(1-0.00392449)*mean(Data)  # 0.2772741


###
### Prior Data ~ 261.00, 1682.00 removed
###

means <- c(116.33, 123.44, 361.25, 76.67, 12.25, 147.67, 
  268.60, 26.40)

vars <- c(9832.33, 9692.03, 136788.92, 7408.33, 122.92, 18497.33, 
  124858.30, 204.80)


###
### Beta prior on p
###

p <- means/vars

mean(p)
var(p)


# p parameters: calculated from mean and variance

estimatebeta <- function(mu, var) {
    alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
    beta <- alpha * (1 / mu - 1)
    return(params = list(alpha = alpha, beta = beta))
}

estimatebeta(mean(p), var(p))

A <- 0.426154
B <- 11.91465


# Prior on p

x <- seq(0.01, 1, length=1000)

fp <- x^(A-1)*(1-x)^(B-1)

plot(x,fp,type="l", main="Beta Prior for p",
    xlab = "Values of p", ylab = "Probability Density")


# prior mode for p

x[fp==max(fp)]  # smaller p gets higher probability ~ no reasonable mode


###
### Gamma prior on r
###

r <- means*p/(1-p)

mean(r)
var(r)


# r parameters: calculated from mean and variance

estimategamma <- function(mu, var) {
    d1 <- mu/v
    d2 <- mu^2/v
    return(params = list(d1 = d1, d2 = d2))
}

estimategamma(mean(r), var(r))

d1 = 1.36369
d2 = 2.00688



# Prior on r

x <- seq(.01, 5, length=1000)

fr <- x^(d2)*exp(-d1*x)

plot(x,fr,type="l", main = "Gamma Prior for r",
    xlab = "Values of gamma", ylab = "Probability Density")


# Mode for r ~ most likely value

r.mode <- x[fr==max(fr)]


#####
#####
##### Posterior Marginal Distribution for r #####


# log PDF ~ otherwise products/summations too large

logfx <- vector(mode = "numeric", length = 1000)

y = 1

for(x in seq(0.1, 2, length=1000)) {
  
  logfx[y] <- (d2-1)*log(x) - d1*x + sum(lgamma(x + Data)) - 
    8*lgamma(x) + lgamma(8*x+A) - lgamma(8*x + A + sum(Data) + B)
  
  y = y + 1
}
  

# actual PDF:

x <- seq(0.1, 2, length=1000)

fx <- exp(logfx)

fx.stand <- fx/sum(fx) # convert to probability distribution (sum=1)

plot(x, fx.stand,type="l", lty=1, 
    xlab = "Values of gamma", ylab = "Probability Density", 
    main = "Marginal for gamma")


# mode for r: 

r.mode <- x[fx == max(fx)]

# 0.5317317



#####
#####
##### Neg. Binom. using mode(r) and p from Dataset #####


dnbinom(Data, size = r.mode, prob=p.data)


x <- seq(1,400)
plot(x,dnbinom(x, size = r.mode, prob=p.data), type="l")


mean.post <- r.mode *(1-p.data)/p.data


#####
#####
##### Determine the constant term of the function #####


# Integrate Marginal Posterior to find the constant

k<-length(Data)
integrand.marginal <- function(r) {
  i<-1
  t<-0
  while(i<=k){
    t<-t+lgamma(r+Data[i])
    i<-i+1
  }
  return(r^(d2-1)*exp(-d1*r)*exp(
    t+lgamma(8*r+A)-8*lgamma(r)-
      lgamma(8*r+sum(Data)+A+B)
  ))
} 

marginal.constant <- integrate(integrand.marginal,lower=0,upper=Inf)

marginal.constant <- (marginal.constant$value)^(-1)



### Posterior Marginal Mean

k<-length(Data)
integrand.mean <- function(r) {
  i<-1
  t<-0
  while(i<=k){
    t<-t+lgamma(r+Data[i])
    i<-i+1
  }
  return(r^(d2)*exp(-d1*r)*exp(
    t-8*lgamma(r)+lgamma(8*r+A-1)+
      lgamma(sum(Data)+B+1)-lgamma(8*r+sum(Data)+A+B)-2500
    ))
} 

# r = 1
# t = 2436.921


marg.mean <- integrate(integrand.mean, lower = 0, upper = Inf) 
marg.mean <- marg.mean$value

marg.mean * marginal.constant * exp(2500)







#####
#####



