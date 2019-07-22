
## =============================================================== ##
## Gaussian Mixture ----
## =============================================================== ##

colvec = 1:3
ltyvec = 1

# Using densities

png("gaus_mix_model.png",width=500,height=400,units="px")
ss = seq(-15,50,.1)
trued = .2*dnorm(ss,1,3) + .8*dnorm(ss,20,5)
yl = c(0,max(dnorm(ss,1,3),dnorm(ss,20,5)))
plot(ss,trued,type="l",
     ylim=yl,xlab="S",ylab="Density",main="Gaussian Mixture, P(Y)=0.2")
points(ss,dnorm(ss,1,3),col="red",type="l")
points(ss,dnorm(ss,20,5),col="green",type="l")
legend(x="topright", legend=c("Mixture P(S)","P(S|Y=0) = N(1,3)","P(S|Y=1) = N(20,5)"), 
       col=colvec, lty=1)
dev.off()

# Using random number generation
# 
set.seed(100)
nreps = 10000
tau = .2    # P(Y=1)


u = runif(nreps)
s = rep(NA,nreps)
s[u<tau] = rnorm(sum(u<tau),1,3) # for controls
s[u>=tau] = rnorm(sum(u>=tau),20,5) # for cases

s0 = rnorm(length(u),1,3)
s1 = rnorm(length(u),20,5)

dl = list(density(s),density(s0),density(s1))
xl = range(c(s,s0,s1))
yl = range(c(dl[[1]]$y,dl[[2]]$y,dl[[3]]$y))


plot(x=0,y=0,type="n",xlim=xl,ylim=yl,xlab="S",ylab="Density",main="Gaussian Mixture, P(Y)=0.2")
for(i in 1:3){
  lines(dl[[i]]$x,dl[[i]]$y,
        col=colvec[i],
        lty=1)
}
legend(x="topright", legend=c("Mixture P(S)","P(S|Y=0)","P(S|Y=1)"), 
       col=colvec, lty=1)
