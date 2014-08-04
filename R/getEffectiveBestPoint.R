# small helper to select yhat(x) + c * s(x) from a design of noisy obs. huang style
# @param design [\code{data.frame}]\cr
#   Design.
# @param model [\code{\link[mlr]{Learner}}]\cr
#   Fitted surrogate model.
# @param par.set [\code{param.set}]\cr
#   Parameter set.
# @param control [\code{\link{MBOControl}}]\cr
#   MBO control object.
# @return [\code{list}]
getEffectiveBestPoint = function(design, model, par.set, control) {
  maximize.mult = ifelse(control$minimize, 1, -1)
  preds = predict(model, newdata = design)

  mu = preds$data$response
  se = preds$data$se

  # FIXME: add this constant to control?
  const = 1

  # minimize mu (if minimization of objective), large se is always penalized
  v = (maximize.mult * mu) + const * se
  j = getMinIndex(v)

  return(list(index = j, des = design[j,, drop = FALSE], mu = mu[[j]], se = se[[j]], val = v[[j]]))
}
