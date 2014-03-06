

library("mnormt")
library("KrigInv") #because I use the "predict" function of KrigInv (which is now the
                    #same as in DiceKriging)

source("R/Scripts/multipointEI/ramunas/multEI_allfunctions.R") #change the source !


set.seed(8)
n <- 6;T <- 80;d <- 2;r <- 2
fun <- branin 
design <- data.frame(maximinLHS(n,k=2))
response <- fun(design) 
model <- km(formula=~1, design = design, response = response,
            covtype="matern3_2")

xx <- runif(r*d);X.new <- matrix(xx,nrow=r)
newdata <- as.data.frame(X.new); colnames(newdata) <- colnames(model@X)
krig  <- predict(object=model, newdata=newdata,type="UK",se.compute=TRUE, cov.compute=TRUE)
mk <- krig$mean
Sigma.r <- krig$cov

t1 <- Sys.time()
for(i in 1:100) aa <- multEI(mu=mk,sigma=Sigma.r,T=T)
t2 <- Sys.time()
difftime(t2,t1)

###### time for 100 calls
#r = 2 : 0.09sec
#r = 3 : 0.29sec
#r = 4 : 1.18sec
#r = 5 : 2.94sec
#r = 6 : 5.64sec
#r = 7 : 11.1sec
#r = 8 : 18.3sec
#r = 9 : 26.3sec
#r = 10 :35sec


#verification with a MC calculation
mychol <- chol(Sigma.r)
nsim <- 100000
white.noise <- rnorm(n=nsim*r)
corr.sim <- crossprod(mychol,matrix(white.noise,nrow=r)) + mk

mean(apply(X=pmax(corr.sim-T,0),MARGIN=2,FUN=max))
multEI(mu=mk,sigma=Sigma.r,T=T)

#example of maximisation of multEI

obj <- multEI.nsteps(model=model,fun=branin,npoints=r,nsteps=1,
                     lower=c(0,0),upper=c(1,1),minimization=TRUE)

obj$par #r points !

