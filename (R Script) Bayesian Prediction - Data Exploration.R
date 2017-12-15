

#####
#####
##### Set up the data #####

kills <- c(11,7,4,62,13,6,396,64)
runtime <- c(100/60, 178/60, 154/60, 118/60, 138/60, 127/60, 153/60, 165/60)
kph <- kills/runtime
kph
rbind(kills, runtime, kph)


###
### The scale is too variant, so we use log(data) instead
###

log(kph)

hist(log(kph))


mean(log(kills))
var(log(kills))

plot(sort(kills))
plot(sort(rpois(8,3)))


#####
#####
##### Compare the Data against a Poisson Distribution #####

###
### Plot our quantiles against a theoretical Poisson
###

qqplot(rpois(8,3),log(kills))
qqplot(rbinom(8, 6, .5  ),log(kills))


# check our raw data against a Poisson model:

p <- ppoints(8)    # 8 equally spaced points on (0,1), excluding endpoints
q <- quantile(kills,p=p) # percentiles of the sample distribution
plot(qpois(p,3) ,q, main="Standard Poisson Q-Q Plot",
    xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=function(x) qpois(x, lambda = 3),col="blue", lty=2)


# check log(data) against a Poisson model:

p <- ppoints(8)    # 8 equally spaced points on (0,1), excluding endpoints
q <- quantile(log(kills),p=p) # percentiles of the sample distribution
plot(qpois(p,3),q, main="Log Poisson Q-Q Plot",
  xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=function(x) qpois(x, lambda = 3), col="blue", lty=2)


# problem: log(data) is not integer valued. 
# If we restrict ourselves to integer values, we will have
# a very limited selection of possible model outputs. 


#####
#####
##### Compare the Data against a Gamma Distribution #####

###
### Kernel Density Estimate for Data
### 

# Kernel plot of our raw data:

plot(density(kills,kernel="gaussian",bw=5)) 


# basically useless: use log instead

plot(density(log(kills),kernel="gaussian",bw=1), main = "Kernel for log(kills)")


###
### Compare our Kernel density plot against various Gammas
###

x <- seq(0, 10, length=100)    # x-axis: plot from 0 to 50, length is the "step" of the graph, want large

plot(density(log(kills),kernel="gaussian",bw=1), main = "Kernel vs. Gamma")

# Altering the shape, fixing scale

shapes <- c(2, 2.5, 3, 3.5, 4)
colors <- c("red", "blue", "pink", "gold", "green")
labels <- c("shape=2", "shape=2.5", "shape=3", "shape=3.5", "shape=4")

for (i in 1:5){
  lines(x, dgamma(x,shape=shapes[i]), lwd=1, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
  labels, lwd=2, lty=c(1, 1, 1, 1, 1), col=colors)


# looks like a great fit for shape ~ 3.5. 
# Of course, Gamma is continuous, and our data is discrete.
# Log(integer functino) is approximately continuous, with high resolution
# both kills and kph ~ kph is a continuous Rv too


###
### Check KPH as well, not just kill totals
###

plot(density(log(kph),kernel="gaussian",bw=1), main = "Kernel vs. Gamma")

shapes <- c(1, 1.5, 2, 2.5, 3, 3.5)
colors <- c("red", "blue", "pink", "gold", "green", "orange")
labels <- c("shape=1", "shape=1.5", "shape=2", "shape=2.5", "shape=3", "shape=3.5")

for (i in 1:6){
  lines(x, dgamma(x,shape=shapes[i], scale=log(59/shapes[i])), lwd=1, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
  labels, lwd=2, col=colors)


#####
#####
##### Compare the Data against a Negative Binomial Distribution #####

# Premise:
# The negative binomial distribution can model overdispersed Poisson data.

x <- seq(0, max(kills))   
hx <- dnbinom(x, mu = 70, size = 1)        
plot(x, hx, type="l", lty=1)

points(x=kills, y=rep(0,length(kills))) # shows our data points against PDF

# It's definitely not a perfect fit, but it looks 
# reasonable enough to proceed.


###
### Plot our quantiles against a theoretical Neg. Binom.
###

qqplot(rnbinom(8, mu=70, size=1), kills)


# check our raw data against a Neg. Binom. model:

p <- ppoints(8)    # 8 equally spaced points on (0,1), excluding endpoints
q <- quantile(kills,p=p) # percentiles of the sample distribution
plot(qnbinom(p,size=1,mu=70), q, main="Standard Neg. Binom. Q-Q Plot",
    xlab="Theoretical Quantiles",ylab="Sample Quantiles")
qqline(q, distribution=function(x) qnbinom(x, mu = 70, size = 1),col="blue", lty=2)


# Pretty decent, minus the one outlier.

# bonus: doesn't require log scale, maintains integer-valued output.



