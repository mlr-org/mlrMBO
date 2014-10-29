
renderPCPPlot = function(data, xnames, alpha, scale = "globalminmax") {
  requirePackages("GGally")

  # ggdata = data[, colnames, drop = FALSE]
  columns = match(xnames, colnames(data))
  args = list(columns = columns, scale = scale)
  if (alpha) {
    data$.alpha = normalize(data$dob, "range", range = c(0, 1))
    args$alphaLines = ".alpha"
  }
  args$data = data
  do.call(ggparcoord, args)
}
