# Additive Bayesian Networks
# Implemented by: H. Achicanoy, 2017

library(abn)
library(rstan)
library(bnlearn)

mydat <- ex0.dag.data[,c("b1","b2","b3","g1","b4","p2","p4")]
## take a subset of cols from dataset ex0.dat.data
## setup distribution list for each node
mydists <- list(b1="binomial", b2="binomial", b3="binomial", g1="gaussian", b4="binomial", p2="poisson", p4="poisson")
## define model
mydag <- matrix(data=c(0,0,1,0,0,0,0,
                       1,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,
                       0,0,0,0,1,0,0,
                       0,0,0,0,0,0,0,
                       0,0,0,0,0,0,0,
                       0,0,0,0,0,0,0), byrow=TRUE, ncol=7)
colnames(mydag) <- rownames(mydag) <- names(mydat)
## now fit the model to calculate its goodness of fit
myres.c <- fitabn(dag.m=mydag, data.df=mydat, data.dists=mydists)
print(myres.c$mlik) ## log marginal likelihood goodness of fit

# Integrate Spain Footbal data from library(plsp)
mydists <- list(ATTACK = "gaussian", DEFENSE = "gaussian", SUCCESS = "gaussian")
mydag <- matrix(data = c(0, 0, 1,
                         0, 0, 1,
                         0, 0, 0), byrow = TRUE, ncol = 3)
colnames(mydag) <- rownames(mydag) <- names(myscores)
myres.c <- fitabn(dag.m=mydag, data.df=myscores, data.dists=mydists)
print(myres.c$mlik) ## log marginal likelihood goodness of fit
