# small helper to select yhat(x) + c * s(x) from a design of noisy obs. huang style
getEffectiveBestPoint = function(design, model, control) {
  maximize.mult = ifelse(control$minimize, 1, -1)
  p = predict(model, newdata = design)

  mu = p$data$response
  se = p$data$se
  # FIXME: add this constant to control?
  const = 1
  # minimize mu (if minimization of objective), large se is always penalized
  v = (maximize.mult * mu) + const * se
  j = getMinIndex(v)
  return(list(index = j, des = design[j,, drop = FALSE], mu = mu[[j]], se = se[[j]], val = v[[j]]))
}



