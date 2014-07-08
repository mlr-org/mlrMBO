# Approximate Knowlwdge gradient
# FIXME: just for kriging at the moment
infillCritAKG = function(points, model, control, par.set, design) {


  maximize.mult = ifelse(control$minimize, 1, -1)

  x=points
  model=model$learner.model
  type="SK"
  new.noise.var = model@covariance@nugget

  if(length(new.noise.var)==0) new.noise.var=0 # nugget.estim=FALSE -> no noise

  newdata.num <- as.numeric(x)
  newdata <- data.frame(t(newdata.num))
  colnames(newdata) = colnames(model@X)
  tau2.new <- new.noise.var
  predx <- predict.km(model, newdata = newdata, type = type,
                      checkNames = FALSE)
  mk.x <- maximize.mult*predx$mean
  sk.x <- predx$sd
  c.x <- predx$c
  V.x <- predx$Tinv.c
  T <- model@T
  z <- model@z
  U <- model@M
  F.x <- model.matrix(model@trend.formula, data = newdata)
  if (sk.x < sqrt(model@covariance@sd2)/1e+06 || model@covariance@sd2 <
        1e-20) {
    AKG <- 0
    tuuinv <- mk.X <- V.X <- mu.x <- cn <- sQ <- Isort <- Iremove <- A1 <- at <- bt <- ct <- NULL
  }
  else {
    predX <- predict.km(model, newdata = model@X, type = type,
                        checkNames = FALSE)
    mk.X <-maximize.mult*predX$mean
    V.X <- predX$Tinv.c
    F.X <- model@F
    m_min <- min(c(mk.X, mk.x))
    if (type == "UK") {
      tuuinv <- solve(t(U) %*% U)
      mu.x <- (F.X - t(V.X) %*% U) %*% tuuinv %*% t(F.x -
                                                      t(V.x) %*% U)
    }
    else {
      tuuinv <- mu.x <- 0
    }
    cn <- c.x - t(V.X) %*% V.x + mu.x
    cn <- c(cn, sk.x^2)
    A <- c(mk.X, mk.x)
    B <- cn/sqrt(tau2.new + sk.x^2)
    sQ <- B[length(B)]
    A <- -A
    nobs <- model@n
    Isort <- order(x = B, y = A)
    b <- B[Isort]
    a <- A[Isort]
    Iremove <- numeric()
    for (i in 1:(nobs)) {
      if (b[i + 1] == b[i]) {
        Iremove <- c(Iremove, i)
      }
    }
    if (length(Iremove) > 0) {
      b <- b[-Iremove]
      a <- a[-Iremove]
    }
    nobs <- length(a) - 1
    C <- rep(0, nobs + 2)
    C[1] <- -1e+36
    C[length(C)] <- 1e+36
    A1 <- 0
    for (k in 2:(nobs + 1)) {
      nondom <- 1
      if (k == nobs + 1) {
        nondom <- 1
      }
      else if ((a[k + 1] >= a[k]) && (b[k] == b[k + 1])) {
        nondom <- 0
      }
      if (nondom == 1) {
        loopdone <- 0
        count <- 0
        while (loopdone == 0 && count < 1000) {
          count <- count + 1
          u <- A1[length(A1)] + 1
          C[u + 1] <- (a[u] - a[k])/(b[k] - b[u])
          if ((length(A1) > 1) && (C[u + 1] <= C[A1[length(A1) -
                                                      1] + 2])) {
            A1 <- A1[-length(A1)]
          }
          else {
            A1 <- c(A1, k - 1)
            loopdone <- 1
          }
        }
      }
    }
    at <- a[A1 + 1]
    bt <- b[A1 + 1]
    ct <- C[c(1, A1 + 2)]
    maxNew <- 0
    for (k in 1:length(at)) {
      maxNew <- maxNew + at[k] * (pnorm(ct[k + 1]) - pnorm(ct[k])) +
        bt[k] * (dnorm(ct[k]) - dnorm(ct[k + 1]))
    }
    AKG <- maxNew - (-m_min)
  } #end of else

  return(-AKG)
}


