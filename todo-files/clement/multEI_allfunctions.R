
pk = function(k, mu, sigma, plugin) {
  q = length(mu)
  pmnorm(
    mu = bk(k, plugin, q) - mk(k, mu),
    sigma = sigmak(k, sigma)
  )
}

pmnorm()

sigmak = function(k, sigma) {
  q = nrow(sigma)
  vk = sigma[, k]
  A = matrix(rep(vk, q), ncol=q)
  sigma - A - t(A)  + sigma[k, k]
}

bk = function(k, plugin, q) {
  b = rep(0, q) 
  b[k] = -plugin
  return(b)
}

mk = function(k, mu) {
  m = mu - mu[k]
  m[k] = - mu[k]
  return(m)
}

pk = function()

#' Calculates the multipoint expected improvement for a bunch of points.
#'
#' @param mu [\code{numeric(k)}]\cr
#'   Mean response value of our k points.
#' @param sigma [\code{matrix(k, k)}]\cr
#'   Covariance matrix of the posterior distribution of our k points.
#' @param plugin [\code{numeric(1)}]\cr
#'   Plugin value for EI formula, e.g. minimum of so far observed points in design.
#' @return [\code{numeric(1)}]. EI value for set of points. 
multEI = function(mu, sigma, plugin, minimization=FALSE) {
  
  r = length(mu)
  proba = new.esp = real.esp = rep(0,times=r)
  sk = sqrt(diag(sigma))
  mult = ifelse(minimize, 1, -1)    
  for(index in 1:r){
    
    #step1
    Gamma.r = sigma
    Gamma.r[index,index] = sigma[index,index]
    for(i in 1:r){
      if(i!=index){
        Gamma.r[index,i] = Gamma.r[i,index] = sigma[index,index] - sigma[index,i]
      }
    }
    for(i in 1:r){
      for(j in i:r){
        if((i!=index)&&(j!=index)){
          Gamma.r[i,j] = Gamma.r[j,i] = sigma[i,j] + sigma[index,index] - sigma[index,i] - sigma[index,j]
        }
      }
    }
    
    
    mu.new = mu - mu[index] 
    mu.new[index] = -mu[index]
    mu.new = -mu.new * mult 
    
    thresholds = rep(0,times=r);thresholds[index] = -T
    thresholds = -thresholds * mult 
    
    #step2
    Gamma.r.norm = cov2cor(Gamma.r)
    

    thresholds.norm = thresholds.center = thresholds-mu.new
    
    for(i in 1:r) thresholds.norm[i] = thresholds.center[i]/sqrt(Gamma.r[i,i])
    
    #step3
    proba[index] = pmnorm(x=thresholds.norm,varcov=Gamma.r.norm,maxpts=r*200)[1]
        
    vect.sum = rep(0,times = r)
    for(j in 1:r){
      vect.sum[j] = Gamma.r.norm[index,j]*Fkz(k=j,sigma=Gamma.r.norm,x=thresholds.norm)/proba[index]
    }
    
    new.esp[index] = 0 - sum(vect.sum)
    real.esp[index] = mu[index] + mult *  new.esp[index] * sk[index]
  }
  
  return(sum(mult * (plugin - real.esp) * proba))
}

Fkz = function(k, sigma, x) {
  
  r = nrow(sigma)
  fact = dnorm(x[k])
  
  #rho_ij.k = (rho_ij - rho_ik * rho_jk)/sqrt( (1-rho_ik^2) (1-rho_jk^2) )

  new.sigma = sigma
  for(i in 1:r){
    for(j in 1:r){
      if((i!=k) && (j!=k)){
        new.sigma[i,j] = (sigma[i,j] - sigma[i,k]*sigma[j,k])/sqrt((1-sigma[i,k]^2)*(1-sigma[j,k]^2))
      }else{
        new.sigma[i,j] = 0
      }
    }
  }
  new.sigma = new.sigma[-k,-k]
  
  new.x = (x-sigma[k,]*x[k])/(sqrt(1-sigma[k,]^2))
  new.x = new.x[-k]
  
  tmp = pmnorm(x = new.x  ,varcov=new.sigma,maxpts=r*200)[1]
  
  return(tmp*fact)
}

multEI_optim2 = function(x,other.points,model,minimization=FALSE){
  x.complete = c(x, other.points)
  return(multEI_optim(x=x.complete,model=model,minimization=minimization))
}

multEI_optim = function(x,model,minimization=FALSE){
  
  d = model@d
  q = length(x)/d
  batchsize = q
  #min distance first:
  n = model@n
  X.new = matrix(x,nrow=d)
  mindist = Inf
  tp1 = c(as.numeric(t(model@X)),x)
  for (i in 1:batchsize){
    #distance between the i^th point and all other points (in the DOE or in the batch)
    xx = X.new[,i]
    tp2=matrix(tp1-as.numeric(xx),ncol=d,byrow=TRUE)^2
    mysums = sqrt(rowSums(tp2))
    mysums[n+i] = Inf #because this one is usually equal to zero...
    mindist = min(mindist,mysums)		
  }
  
  if(mindist < 1e-5) return(0)
  
  if(minimization){ 
    T = min(model@y)
  }else{
    T = max(model@y)
  }
    
  X.new = t(matrix(x,nrow=d))
  X.new = as.data.frame(X.new);colnames(X.new) = colnames(model@X)
  
  pred = predict_nobias_km(object=model,newdata=X.new,type="UK",
                            se.compute=TRUE,cov.compute=TRUE)
  
  mu = pred$mean
  sigma = pred$cov
  
  if(q==1){
    sk = sqrt(sigma)
    tmp = (mu - T)/sk
    if(minimization){
      tmp = -tmp  
      res = sk*(tmp*pnorm(tmp)+dnorm(tmp))
    }else{
      res = sk*(tmp*pnorm(tmp)+dnorm(tmp))
    }
    if(is.nan(res)) res = 0
    return(res)
  }else{
    mEI = multEI(mu=mu,sigma=sigma,T=T,minimization=minimization)
    if(is.nan(mEI)) mEI = 0
    return(mEI)
  }
}

max_multEI = function(model,control,batchsize,lower,upper,minimization=FALSE){
  
  d = ncol(model@X)
  if (is.null(control$pop.size)) 
    control$pop.size = floor(4 + 3 * log(d))
  if (is.null(control$max.generations)) 
    control$max.generations = 5
  if (is.null(control$wait.generations)) 
    control$wait.generations = 2
  if (is.null(control$BFGSburnin)) 
    control$BFGSburnin = 0
  if (is.null(control$optimoption)) 
    control$optimoption = 2
  if (is.null(control$parinit)) 
    control$parinit = NULL
  
  optimoption = control$optimoption
  
  if(optimoption==1){
    
    domaine = cbind(
      rep(lower, times = batchsize), 
      rep(upper, times = batchsize)
    )
    
    o = genoud(multEI_optim, nvars = (d*batchsize), max = TRUE, 
                pop.size = control$pop.size, 
                max.generations = control$max.generations, 
                wait.generations = control$wait.generations, 
                hard.generation.limit = TRUE, starting.values = control$parinit, 
                MemoryMatrix = TRUE, Domains = domaine, default.domains = 10, 
                solution.tolerance = 1e-09, boundary.enforcement = 2, 
                lexical = FALSE, gradient.check = FALSE, BFGS = TRUE, 
                data.type.int = FALSE, hessian = FALSE, 
                unif.seed = floor(runif(1, max = 10000)), 
                int.seed = floor(runif(1, max = 10000)), 
                print.level = 1, share.type = 0, instance.number = 0, 
                output.path = "stdout", output.append = FALSE, project.path = NULL, 
                P1 = 50, P2 = 50, P3 = 50, P4 = 50, P5 = 50, P6 = 50, 
                P7 = 50, P8 = 50, P9 = 0, P9mix = NULL, BFGSburnin = control$BFGSburnin, 
                BFGSfn = NULL, BFGShelp = NULL, cluster = FALSE, balance = FALSE, 
                debug = FALSE, model = model, minimization=minimization)
    o$par = t(matrix(o$par,nrow=d))
    colnames(o$par) = colnames(model@X)
    o$value = as.matrix(o$value)
    colnames(o$value) = "EI"
    return(list(par = o$par, value = o$value))
  }else{
  
    domaine = cbind(lower, upper)
    other.points = NULL
    for (i in 1:batchsize) {
      o = genoud(multEI_optim2, nvars = d, max = TRUE, 
                  pop.size = control$pop.size, 
                  max.generations = control$max.generations, 
                  wait.generations = control$wait.generations, 
                  hard.generation.limit = TRUE, starting.values = control$parinit, 
                  MemoryMatrix = TRUE, Domains = domaine, default.domains = 10, 
                  solution.tolerance = 1e-09, boundary.enforcement = 2, 
                  lexical = FALSE, gradient.check = FALSE, BFGS = TRUE, 
                  data.type.int = FALSE, hessian = FALSE, 
                  unif.seed = floor(runif(1, max = 10000)), 
                  int.seed = floor(runif(1, max = 10000)), 
                  print.level = 1, share.type = 0, instance.number = 0, 
                  output.path = "stdout", output.append = FALSE, project.path = NULL, 
                  P1 = 50, P2 = 50, P3 = 50, P4 = 50, P5 = 50, P6 = 50, 
                  P7 = 50, P8 = 50, P9 = 0, P9mix = NULL, BFGSburnin = control$BFGSburnin, 
                  BFGSfn = NULL, BFGShelp = NULL, cluster = FALSE, balance = FALSE, 
                  debug = FALSE, 
                  model = model, 
                  minimization=minimization,
                  other.points=other.points)
      
      other.points = c(other.points, as.numeric(o$par))
    }
    o$par = t(matrix(other.points, nrow = d))
    colnames(o$par) = colnames(model@X)
    o$value = as.matrix(o$value)
    colnames(o$value) = "EI"
    return(list(par = o$par, value = o$value))
  }
}

multEI.nsteps = function (model, fun, npoints, nsteps, lower, upper, 
                           kmcontrol = NULL, control = NULL,minimization=FALSE) 
{
  n = nrow(model@X)
  if (length(kmcontrol$penalty) == 0) 
    kmcontrol$penalty = model@penalty
  if (length(kmcontrol$optim.method) == 0) 
    kmcontrol$optim.method = model@optim.method
  if (length(kmcontrol$parinit) == 0) 
    kmcontrol$parinit = model@parinit
  if (length(kmcontrol$control) == 0) 
    kmcontrol$control = model@control
  kmcontrol$control$trace = FALSE
  for (i in 1:nsteps) {
    #L = min(model@y)
    res.multEI = max_multEI(model=model,control=control,batchsize= npoints, 
                                lower = lower, upper = upper, 
                                minimization=minimization)
    model@X = rbind(model@X, res.multEI$par)
    model@y = rbind(model@y, as.matrix(apply(res.multEI$par, 
                                              1, fun), npoints))
    model = km(formula = model@trend.formula, design = model@X, 
                response = model@y, covtype = model@covariance@name, 
                lower = model@lower, upper = model@upper, nugget = NULL, 
                penalty = kmcontrol$penalty, optim.method = kmcontrol$optim.method, 
                parinit = kmcontrol$parinit, control = kmcontrol$control, 
                gr = model@gr)
  }
  return(list(
              par = model@X[(n + 1):(n + nsteps * npoints), , drop = FALSE], 
              value = model@y[(n + 1):(n + nsteps * npoints), , drop = FALSE], 
              npoints = npoints, 
              nsteps = nsteps, 
              lastmodel = model
              )
         )
}




